module LocalRepositoryPage exposing (..)

import Aptly.Local.Repository
import Html
import Http

type alias Model =
    { repositories : List Aptly.Local.Repository.Repository
    , state : State
    }

type Msg
    = List (Result Http.Error (List Aptly.Local.Repository.Repository))
    | State State


type State
    = Listing
    | Editing Aptly.Local.Repository.Repository

init : (Model, Cmd Msg)
init =
    (Model [] Listing, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        List (Err _) ->
            (model, Cmd.none)

        List (Ok repositories) ->
            ({ model | repositories = repositories }, Cmd.none)

        State Listing ->
            ({ model | state = Listing }, Cmd.none)

        State (Editing repository) ->
            ({ model | state = Editing repository }, Cmd.none)

view : Model -> Html.Html Msg
view model =
    Html.div []
        <| List.append
            [ Html.h1 [] [ Html.text "Local Repositories" ]
            ]
            <| case model.state of
                Listing ->
                    (List.intersperse (Html.hr [] []) <| List.map (Aptly.Local.Repository.view (\repository -> State (Editing repository))) model.repositories)

                Editing repository ->
                    [ Aptly.Local.Repository.viewForm False repository ]
