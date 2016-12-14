module Aptly.Local.RepositoryList exposing (..)

import Aptly.Config
import Aptly.Generic.List
import Aptly.Local.Repository
import Html

type Msg
    = ListMsg (Aptly.Generic.List.Msg Aptly.Local.Repository.Repository Aptly.Local.Repository.Msg)
    | Force Bool

type alias RepositoryList =
    { config : Aptly.Config.Config
    , list : Aptly.Generic.List.Model Aptly.Local.Repository.Repository
    , force : Bool
    }

factory : Bool -> String -> Aptly.Generic.List.RequestFactory Aptly.Local.Repository.Repository Aptly.Local.Repository.Msg
factory force server =
    { create = Just <| (Aptly.Local.Repository.createCreateRequest server, Aptly.Local.Repository.viewForm True)
    , delete = Just <| (Aptly.Local.Repository.createDeleteRequest force server, Aptly.Local.Repository.viewConfirmation force)
    , edit = Just <| (Aptly.Local.Repository.createEditRequest server, Aptly.Local.Repository.viewForm False)
    , list = (Aptly.Local.Repository.createListRequest server, Aptly.Local.Repository.view)
    }

init : Aptly.Config.Config -> (RepositoryList, Cmd Msg)
init config =
    let
        (listModel, listMsg) =
            Aptly.Generic.List.init (factory False config.server)
    in
        (RepositoryList config listModel False, Cmd.map ListMsg listMsg)

update : Msg -> RepositoryList -> (RepositoryList, Cmd Msg)
update msg model =
    case msg of
        ListMsg msg ->
            let
                (listModel, listMsg) =
                    Aptly.Generic.List.update Aptly.Local.Repository.update (factory model.force model.config.server) msg model.list
            in
                ({ model | list = listModel }, Cmd.map ListMsg listMsg)

        Force force ->
            ({ model | force = force }, Cmd.none)

view : RepositoryList -> Html.Html Msg
view model =
    Html.map ListMsg <| Aptly.Generic.List.view (factory model.force model.config.server) (Aptly.Local.Repository.Repository "" "" "" "") model.list
