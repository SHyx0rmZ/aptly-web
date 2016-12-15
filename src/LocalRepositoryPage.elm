module LocalRepositoryPage exposing (..)

import Aptly.Config
import Aptly.Local.RepositoryList
import Html

type alias Model =
    { config : Aptly.Config.Config
    , repositoryList : Aptly.Local.RepositoryList.RepositoryList
    }

type Msg
    = RepositoryListMsg Aptly.Local.RepositoryList.Msg

init : Aptly.Config.Config -> (Model, Cmd Msg)
init config =
    let
        (repositoryListModel, repositoryListMsg) =
            Aptly.Local.RepositoryList.init config
    in
        (Model config repositoryListModel, Cmd.map RepositoryListMsg repositoryListMsg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RepositoryListMsg msg ->
            let
                (repositoryListModel, repositoryListMsg) =
                    Aptly.Local.RepositoryList.update msg model.repositoryList
            in
                ({ model | repositoryList = repositoryListModel }, Cmd.map RepositoryListMsg repositoryListMsg)

view : Model -> Html.Html Msg
view model =
    Html.map RepositoryListMsg <| Aptly.Local.RepositoryList.view model.repositoryList
