module Aptly.SnapshotList exposing (..)

import Aptly.Config
import Aptly.Decode
import Aptly.Generic.List
import Aptly.Snapshot
import Aptly.SnapshotListSynchronizer
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Task

type State
    = Listing
    | DeletingMultiple (List String)

type Msg
    = ListMsg (Aptly.Generic.List.Msg Aptly.Snapshot.Snapshot Aptly.Snapshot.Msg Msg)
    | Delete (List String)
    | Force Bool
    | ListModification (Aptly.Generic.List.Modification Aptly.Snapshot.Snapshot)
    | State State
    | Select Json.Decode.Value

type alias SnapshotList =
    { config : Aptly.Config.Config
    , list : Aptly.Generic.List.Model Aptly.Snapshot.Snapshot
    , force : Bool
    , state : State
    }

factory : Bool -> String -> Aptly.Generic.List.RequestFactory Aptly.Snapshot.Snapshot Aptly.Snapshot.Msg Msg
factory force server =
    { create = Nothing
    , delete = Just (Aptly.Snapshot.createDeleteRequest force server, Aptly.Snapshot.viewConfirmation force (\force -> Aptly.Generic.List.mapMsg <| Force force))
    , edit = Just (Aptly.Snapshot.createEditRequest server, Aptly.Snapshot.viewForm)
    , list = (Aptly.Snapshot.createListRequest server, Aptly.Snapshot.view)
    , listExtra = Just [ ("Delete multiple snapshots", State <| DeletingMultiple []) ]
    }

init : Aptly.Config.Config -> (SnapshotList, Cmd Msg)
init config =
    let
        (listModel, listMsg) =
            Aptly.Generic.List.init (factory False config.server)
    in
        (SnapshotList config listModel False Listing, Cmd.map ListMsg listMsg)

items : SnapshotList -> List Aptly.Snapshot.Snapshot
items snapshotList =
    Aptly.Generic.List.items snapshotList.list

subscriptions : SnapshotList -> Sub Msg
subscriptions model =
    Aptly.SnapshotListSynchronizer.onModify ListModification

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

        Delete snapshots ->
            ({ model | state = Listing, force = False }, Cmd.batch <| List.map (\snapshot -> Task.perform (\() -> ListMsg <| Aptly.Generic.List.Request <| Aptly.Generic.List.Deleting snapshot) <| Task.succeed ())  <| List.concat <| List.map (\name -> List.filterMap (\snapshot -> if name == snapshot.name then Just snapshot else Nothing) <| Aptly.Generic.List.items model.list) snapshots)

        Force force ->
            ({ model | force = force }, Cmd.none)

        State state ->
            ({ model | state = state }, Cmd.none)

        Select value ->
            case model.state of
                DeletingMultiple _ ->
                    ({ model | state = DeletingMultiple <| Aptly.Decode.decodeOptionsCollection value }, Cmd.none)

                Listing ->
                    (model, Cmd.none)

view : SnapshotList -> Html.Html Msg
view model =
    case model.state of
        Listing ->
            Html.map ListMsg <| Aptly.Generic.List.view (factory model.force model.config.server) (Aptly.Snapshot.Snapshot "" "" "") "Snapshots" model.list

        DeletingMultiple snapshots ->
            Html.div []
                [ Html.h1 [] [ Html.text "Snapshots" ]
                , Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]
                , Html.button [ Html.Events.onClick <| Delete snapshots ] [ Html.text "Delete" ]
                , Html.hr [] []
                , Html.select [ onSelect <| Select, Html.Attributes.multiple True, Html.Attributes.size (min 10 <| List.length <| Aptly.Generic.List.items model.list) ]
                    <| List.map (\snapshot -> Html.option [ Html.Attributes.selected <| List.member snapshot.name snapshots ] [ Html.text snapshot.name ]) <| Aptly.Generic.List.items model.list
                ]

onSelect : (Json.Decode.Value -> msg) -> Html.Attribute msg
onSelect tagger =
    Html.Events.on "change" (Json.Decode.map tagger <| Json.Decode.at [ "target", "selectedOptions" ] Json.Decode.value)
