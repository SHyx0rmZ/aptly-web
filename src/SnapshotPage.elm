module SnapshotPage exposing (..)

import Aptly.Config
import Aptly.Snapshot
import Html
import Html.Attributes
import Html.Events
import Http

type alias Model =
    { config : Aptly.Config.Config
    , snapshots : List Aptly.Snapshot.Snapshot
    , state : State
    , force : Bool
    }

type alias ChangeSet =
    { old : Maybe Aptly.Snapshot.Snapshot
    , new : Maybe Aptly.Snapshot.Snapshot
    }

type State
    = Listing
    | Changing (ChangeSet)

type Msg
    = State State
    | List (Result Http.Error (List Aptly.Snapshot.Snapshot))
    | Delete Aptly.Snapshot.Snapshot (Result Http.Error String)
    | Request ((Result Http.Error String) -> Msg) (Http.Request String)
    | Force Bool

deleteMsg : String -> Bool -> Aptly.Snapshot.Snapshot -> Msg
deleteMsg server force snapshot =
    Request (Delete snapshot) <| Aptly.Snapshot.createDeleteRequest server force snapshot

init : Aptly.Config.Config -> (Model, Cmd Msg)
init config =
    (Model config [] Listing False, Aptly.Snapshot.createListRequest config.server |> Http.send List)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        State Listing ->
            ({ model | state = Listing, force = False }, Cmd.none)

        State (Changing changeSet) ->
            ({ model | state = Changing changeSet }, Cmd.none)

        List (Err _) ->
            (model, Cmd.none)

        List (Ok snapshots) ->
            ({ model | snapshots = List.sortBy .createdAt snapshots |> List.reverse }, Cmd.none)

        Delete _ (Err _) ->
            (model, Cmd.none)

        Delete oldSnapshot (Ok _) ->
            ({ model | state = Listing, force = False, snapshots = List.filter (\snapshot -> snapshot /= oldSnapshot) model.snapshots }, Cmd.none)

        Request result request ->
            (model, Http.send result request)

        Force force ->
            ({ model | force = force }, Cmd.none)

view : Model -> Html.Html Msg
view model =
    Html.div []
        <| List.append
            [ Html.h1 [] [ Html.text "Snapshots" ]
            ]
            <| case model.state of
                Listing ->
                    List.intersperse (Html.hr [] [])
                        <| List.map
                            ( Aptly.Snapshot.view
                                (\snapshot -> State <| Changing <|  ChangeSet (Just snapshot) Nothing)
                            )
                            model.snapshots

                Changing changeSet ->
                    case (changeSet.old, changeSet.new) of
                        (Just oldSnapshot, Nothing) ->
                            [ Html.p [] [ Html.text <| "Are you sure you want to delete the snapshot \"" ++ oldSnapshot.name ++ "\"?" ]
                            , Html.strong [] [ Html.text "Warning!" ]
                            , Html.text " This action cannot be undone!"
                            , Html.div []
                                [ Html.input [ Html.Events.onClick <| Force <| not model.force, Html.Attributes.type_ "checkbox", Html.Attributes.checked model.force ] []
                                , Html.text "Force"
                                ]
                            , Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]
                            , Html.button [ Html.Events.onClick <| (deleteMsg model.config.server model.force oldSnapshot) ] [ Html.text "Delete" ]
                            ]

                        _ ->
                            []
