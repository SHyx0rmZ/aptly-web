module PublishedRepositoryPage exposing (..)

import Aptly.Config
import Aptly.Published.RepositoryList
import Aptly.SnapshotList
import Html

type alias Model =
    { config : Aptly.Config.Config
    , repositoryList : Aptly.Published.RepositoryList.RepositoryList
    , snapshotList : Aptly.SnapshotList.SnapshotList
    }

type Msg
    = RepositoryListMsg Aptly.Published.RepositoryList.Msg
    | SnapshotListMsg Aptly.SnapshotList.Msg

init : Aptly.Config.Config -> (Model, Cmd Msg)
init config  =
    let
        (repositoryListModel, repositoryListMsg) =
            Aptly.Published.RepositoryList.init config

        (snapshotListModel, snapshotListMsg) =
            Aptly.SnapshotList.init config
    in
        (Model config repositoryListModel snapshotListModel, Cmd.batch
            [ Cmd.map RepositoryListMsg repositoryListMsg
            , Cmd.map SnapshotListMsg snapshotListMsg
            ])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RepositoryListMsg msg ->
            let
                (repositoryListModel, repositoryListMsg) =
                    Aptly.Published.RepositoryList.update msg model.repositoryList
            in
                ({ model | repositoryList = repositoryListModel }, Cmd.map RepositoryListMsg repositoryListMsg)

        SnapshotListMsg msg ->
            let
                (snapshotListModel, snapshotListMsg) =
                    Aptly.SnapshotList.update msg model.snapshotList
            in
                ({ model | snapshotList = snapshotListModel }, Cmd.map SnapshotListMsg snapshotListMsg)

view : Model -> Html.Html Msg
view model =
    Html.map RepositoryListMsg <| Aptly.Published.RepositoryList.view model.repositoryList
