module SnapshotPage exposing (..)

import Aptly.Config
import Aptly.Snapshot
import Html
import Http

type alias Model =
    { config : Aptly.Config.Config
    , snapshots : List Aptly.Snapshot.Snapshot
    , state : State
    }

type State
    = Listing

type Msg
    = State State
    | List (Result Http.Error (List Aptly.Snapshot.Snapshot))

init : Aptly.Config.Config -> (Model, Cmd Msg)
init config =
    (Model config [] Listing, Aptly.Snapshot.createListRequest config.server |> Http.send List)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        State Listing ->
            ({ model | state = Listing }, Cmd.none)

        List (Err _) ->
            (model, Cmd.none)

        List (Ok snapshots) ->
            ({ model | snapshots = List.sortBy .name snapshots }, Cmd.none)

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
                            )
                            model.snapshots
