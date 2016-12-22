module PublishedRepositoryPage exposing (..)

import Aptly.Config
import Aptly.Published.RepositoryList
import Html

type alias Model =
    { config : Aptly.Config.Config
    , repositoryList : Aptly.Published.RepositoryList.RepositoryList
    }

type Msg
    = RepositoryListMsg Aptly.Published.RepositoryList.Msg

init : Aptly.Config.Config -> (Model, Cmd Msg)
init config  =
    let
        (repositoryListModel, repositoryListMsg) =
            Aptly.Published.RepositoryList.init config
    in
        (Model config repositoryListModel, Cmd.map RepositoryListMsg repositoryListMsg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RepositoryListMsg msg ->
            let
                (repositoryListModel, repositoryListMsg) =
                    Aptly.Published.RepositoryList.update msg model.repositoryList
            in
                ({ model | repositoryList = repositoryListModel }, Cmd.map RepositoryListMsg repositoryListMsg)

view : Model -> Html.Html Msg
view model =
    Html.map RepositoryListMsg <| Aptly.Published.RepositoryList.view model.repositoryList

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map RepositoryListMsg <| Aptly.Published.RepositoryList.subscriptions model.repositoryList
