module SnapshotPage exposing (..)

import Aptly.Config
import Aptly.SnapshotList
import Html

type alias Model =
    { config : Aptly.Config.Config
    , snapshotList : Aptly.SnapshotList.SnapshotList
    }

type Msg
    = SnapshotListMsg Aptly.SnapshotList.Msg

init : Aptly.Config.Config -> (Model, Cmd Msg)
init config =
    let
        (snapshotListModel, snapshotListMsg) =
            Aptly.SnapshotList.init config
    in
        (Model config snapshotListModel, Cmd.map SnapshotListMsg snapshotListMsg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SnapshotListMsg msg ->
            let
                (snapshotListModel, snapshotListMsg) =
                    Aptly.SnapshotList.update msg model.snapshotList
            in
                ({ model | snapshotList = snapshotListModel }, Cmd.map SnapshotListMsg snapshotListMsg)

view : Model -> Html.Html Msg
view model =
    Html.map SnapshotListMsg <| Aptly.SnapshotList.view model.snapshotList
