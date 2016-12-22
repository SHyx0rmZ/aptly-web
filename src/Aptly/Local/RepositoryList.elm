module Aptly.Local.RepositoryList exposing (..)

import Aptly.Config
import Aptly.Generic.List
import Aptly.Local.Repository
import Aptly.Snapshot
import Aptly.SnapshotListSynchronizer
import Html
import Html.Attributes
import Html.Events
import Http
import Task
import Time

type Transition
    = WaitingForTime
    | Editing Aptly.Snapshot.Snapshot

type State
    = Listing
    | CreatingSnapshot Transition Aptly.Local.Repository.Repository

type Msg
    = ListMsg (Aptly.Generic.List.Msg Aptly.Local.Repository.Repository Aptly.Local.Repository.Msg Msg)
    | Force Bool
    | State State
    | Time Time.Time
    | RequestSnapshot Aptly.Local.Repository.Repository Aptly.Snapshot.Snapshot
    | CreateSnapshot (Result Http.Error Aptly.Snapshot.Snapshot)

type alias RepositoryList =
    { config : Aptly.Config.Config
    , list : Aptly.Generic.List.Model Aptly.Local.Repository.Repository
    , force : Bool
    , state : State
    }

factory : Bool -> String -> Aptly.Generic.List.RequestFactory Aptly.Local.Repository.Repository Aptly.Local.Repository.Msg Msg
factory force server =
    { create = Just <| (Aptly.Local.Repository.createCreateRequest server, Aptly.Local.Repository.viewForm True)
    , delete = Just <| (Aptly.Local.Repository.createDeleteRequest force server, Aptly.Local.Repository.viewConfirmation force (Aptly.Generic.List.mapMsg << Force))
    , edit = Just <| (Aptly.Local.Repository.createEditRequest server, Aptly.Local.Repository.viewForm False)
    , list = (Aptly.Local.Repository.createListRequest server, Aptly.Local.Repository.view <| Aptly.Generic.List.mapMsg << State << CreatingSnapshot WaitingForTime)
    }

init : Aptly.Config.Config -> (RepositoryList, Cmd Msg)
init config =
    let
        (listModel, listMsg) =
            Aptly.Generic.List.init (factory False config.server)
    in
        (RepositoryList config listModel False Listing, Cmd.batch
            [ Cmd.map ListMsg listMsg
            ])

subscriptions : RepositoryList -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> RepositoryList -> (RepositoryList, Cmd Msg)
update msg model =
    case msg of
        ListMsg (Aptly.Generic.List.ParentMsg msg) ->
            (model, Task.perform (\() -> msg) <| Task.succeed ())

        ListMsg msg ->
            let
                (listModel, listMsg) =
                    Aptly.Generic.List.update Aptly.Local.Repository.update (factory model.force model.config.server) msg model.list
            in
                ({ model | list = listModel }, Cmd.map ListMsg listMsg)

        Force force ->
            ({ model | force = force }, Cmd.none)

        State Listing ->
            ({ model | state = Listing }, Cmd.none)

        State (CreatingSnapshot transition repository) ->
            case transition of
                WaitingForTime ->
                    ({ model | state = CreatingSnapshot transition repository }, Task.perform Time Time.now)

                Editing snapshot ->
                    ({ model | state = CreatingSnapshot (Editing snapshot) repository }, Cmd.none)

        Time time ->
            case model.state of
                CreatingSnapshot WaitingForTime repository ->
                    ({ model | state = CreatingSnapshot (Editing <| Aptly.Snapshot.Snapshot (repository.name ++ "-" ++ (toString <| floor <| Time.inSeconds time)) ("Snapshot from local repo [" ++ repository.name ++ "]") "") repository }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        RequestSnapshot repository snapshot ->
            (model, Http.send CreateSnapshot <| Aptly.Snapshot.createCreateRequest repository.name model.config.server snapshot)

        CreateSnapshot (Err _) ->
            (model, Cmd.none)

        CreateSnapshot (Ok snapshot) ->
            ({ model | state = Listing }, Aptly.SnapshotListSynchronizer.modify <| Aptly.Generic.List.Create (Ok snapshot))

view : RepositoryList -> Html.Html Msg
view model =
    case model.state of
        Listing ->
            Html.map ListMsg <| Aptly.Generic.List.view (factory model.force model.config.server) (Aptly.Local.Repository.Repository "" "" "" "") "Local Repositories" model.list

        CreatingSnapshot transition repository ->
            case transition of
                WaitingForTime ->
                    Html.div []
                        [ Html.h1 [] [ Html.text "Creating snapshot" ]
                        , Html.hr [] []
                        , Html.p [] [ Html.text "Waiting for current time..." ]
                        , Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]
                        ]

                Editing snapshot ->
                    Html.div []
                        [ Html.h1 [] [ Html.text "Creating snapshot" ]
                        , Html.hr [] []
                        , Html.label []
                            [ Html.text "Name"
                            , Html.input [ Html.Attributes.value <| snapshot.name ] []
                            ]
                        , Html.br [] []
                        , Html.label []
                            [ Html.text "Description"
                            , Html.input [ Html.Attributes.value <| snapshot.description ] []
                            ]
                        , Html.br [] []
                        , Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]
                        , Html.button [ Html.Events.onClick <| RequestSnapshot repository snapshot ] [ Html.text "Create" ]
                        ]
