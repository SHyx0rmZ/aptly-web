module Aptly.SnapshotList exposing (..)

import Aptly.Config
import Aptly.Generic.List
import Aptly.Snapshot
import Aptly.SnapshotListSynchronizer
import Html
import Task

type Msg
    = ListMsg (Aptly.Generic.List.Msg Aptly.Snapshot.Snapshot Aptly.Snapshot.Msg Msg)
    | Force Bool
    | ListModification (Aptly.Generic.List.Modification Aptly.Snapshot.Snapshot)

type alias SnapshotList =
    { config : Aptly.Config.Config
    , list : Aptly.Generic.List.Model Aptly.Snapshot.Snapshot
    , force : Bool
    }

factory : Bool -> String -> Aptly.Generic.List.RequestFactory Aptly.Snapshot.Snapshot Aptly.Snapshot.Msg Msg
factory force server =
    { create = Nothing
    , delete = Just (Aptly.Snapshot.createDeleteRequest force server, Aptly.Snapshot.viewConfirmation force (\force -> Aptly.Generic.List.mapMsg <| Force force))
    , edit = Just (Aptly.Snapshot.createEditRequest server, Aptly.Snapshot.viewForm)
    , list = (Aptly.Snapshot.createListRequest server, Aptly.Snapshot.view)
    }

init : Aptly.Config.Config -> (SnapshotList, Cmd Msg)
init config =
    let
        (listModel, listMsg) =
            Aptly.Generic.List.init (factory False config.server)
    in
        (SnapshotList config listModel False, Cmd.map ListMsg listMsg)

items : SnapshotList -> List Aptly.Snapshot.Snapshot
items snapshotList =
    Aptly.Generic.List.items snapshotList.list

update : Msg -> SnapshotList -> (SnapshotList, Cmd Msg)
update msg model =
    case msg of
        ListModification modification ->
            let
                (listModel, listMsg) =
                    Aptly.Generic.List.update Aptly.Snapshot.update (factory model.force model.config.server) (Aptly.Generic.List.Modify modification) model.list
            in
                ({ model | list = listModel }, Cmd.map ListMsg listMsg)

        ListMsg (Aptly.Generic.List.ParentMsg msg) ->
            (model, Task.perform (\() -> msg) <| Task.succeed ())

        ListMsg msg ->
            case msg of
                Aptly.Generic.List.Modify modification ->
                    (model, Aptly.SnapshotListSynchronizer.modify modification)

                _ ->
                    let
                        (listModel, listMsg) =
                            Aptly.Generic.List.update Aptly.Snapshot.update (factory model.force model.config.server) msg model.list
                    in
                        ({ model | list = listModel }, Cmd.map ListMsg listMsg)

        Force force ->
            ({ model | force = force }, Cmd.none)

view : SnapshotList -> Html.Html Msg
view model =
    Html.map ListMsg <| Aptly.Generic.List.view (factory model.force model.config.server) (Aptly.Snapshot.Snapshot "" "" "") "Snapshots" model.list

subscriptions : SnapshotList -> Sub Msg
subscriptions model =
    Aptly.SnapshotListSynchronizer.onModify ListModification
