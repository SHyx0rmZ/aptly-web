module Aptly.Published.RepositoryList exposing (..)

import Aptly.Config
import Aptly.Generic.List
import Aptly.Published.Repository
import Aptly.SnapshotList
import Html
import Task

type alias RepositoryList =
    { config : Aptly.Config.Config
    , list : Aptly.Generic.List.Model Aptly.Published.Repository.Repository
    , force : Bool
    }

type Msg
    = ListMsg (Aptly.Generic.List.Msg Aptly.Published.Repository.Repository Aptly.Published.Repository.Msg Msg)
    | Force Bool

factory : Bool -> String -> Aptly.Generic.List.RequestFactory Aptly.Published.Repository.Repository Aptly.Published.Repository.Msg Msg
factory force server =
    { create = Nothing
    , delete = Just (Aptly.Published.Repository.createDeleteRequest force server, Aptly.Published.Repository.viewConfirmation force (\force -> Aptly.Generic.List.mapMsg (Force force)))
    , edit = Just (Aptly.Published.Repository.createEditRequest server, Aptly.Published.Repository.viewForm)
    , list = (Aptly.Published.Repository.createListRequest server, Aptly.Published.Repository.view)
    }

init : Aptly.Config.Config -> (RepositoryList, Cmd Msg)
init config =
    let
        (listModel, listMsg) =
            Aptly.Generic.List.init (factory False config.server)
    in
        (RepositoryList config listModel False, Cmd.map ListMsg listMsg)

update : Msg -> RepositoryList -> (RepositoryList, Cmd Msg)
update msg repositoryList =
    case msg of
        ListMsg (Aptly.Generic.List.ParentMsg msg) ->
            (repositoryList, Task.perform (\() -> msg) <| Task.succeed ())

        ListMsg (Aptly.Generic.List.State (Aptly.Generic.List.Editing oldRepository newRepository)) ->
            let
                (snapshotListModel, snapshotListMsg) =
                    Aptly.SnapshotList.init repositoryList.config

                newMsg =
                    Aptly.Generic.List.State (Aptly.Generic.List.Editing oldRepository ({ newRepository | snapshotList = Just snapshotListModel }))
                        |> Debug.log "newMsg"

                (listModel, listMsg) =
                    Aptly.Generic.List.update Aptly.Published.Repository.update (factory repositoryList.force repositoryList.config.server) newMsg repositoryList.list
                        |> Debug.log "list"
            in
                ({ repositoryList | list = listModel }, Cmd.batch
                    [ Cmd.map ListMsg listMsg
                    , Cmd.map (\msg -> ListMsg <| Aptly.Generic.List.ItemMsg <| Aptly.Published.Repository.SnapshotListMsg msg) snapshotListMsg
                    ])

        ListMsg msg ->
            let
                (listModel, listMsg) =
                    Aptly.Generic.List.update Aptly.Published.Repository.update (factory repositoryList.force repositoryList.config.server) msg repositoryList.list
            in
                ({ repositoryList | list = listModel }, Cmd.map ListMsg listMsg)

        Force force ->
            ({ repositoryList | force = force }, Cmd.none)

view : RepositoryList -> Html.Html Msg
view repositoryList =
    Html.map ListMsg <| Aptly.Generic.List.view (factory repositoryList.force repositoryList.config.server) (Aptly.Published.Repository.Repository Nothing "" "" "" Aptly.Published.Repository.Local [] [] "" "" Nothing) "Published Repositories" repositoryList.list

subscriptions : RepositoryList -> Sub Msg
subscriptions repositoryList =
    Sub.map ListMsg <| Aptly.Generic.List.subscriptions Aptly.Published.Repository.subscriptions repositoryList.list
