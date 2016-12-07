module PublishedRepositoryPage exposing (..)

import Aptly.Published.Repository
import Html
import Http

type alias Model =
    { repositories : List Aptly.Published.Repository.Repository
    }

type Msg
    = List (Result Http.Error (List Aptly.Published.Repository.Repository))

init =
    (Model [], Cmd.none)

update msg model =
    case msg of
        List (Err _) ->
            (model, Cmd.none)

        List (Ok repositories) ->
            ({ model | repositories = repositories }, Cmd.none)

view model =
    Html.div []
        <| List.append
            [ Html.h1 [] [ Html.text "Published Repositories" ]
            ]
            (List.map Aptly.Published.Repository.view model.repositories)
