module PublishedRepositoryPage exposing (..)

import Aptly.Published.Repository
import Html
import Http

type alias Model =
    { repositories : List Aptly.Published.Repository.Repository
    , server : String
    }

type Msg
    = List (Result Http.Error (List Aptly.Published.Repository.Repository))

init : String -> (Model, Cmd Msg)
init server =
    (Model [] server, Aptly.Published.Repository.createListRequest server |> Http.send List)

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
            <| List.intersperse (Html.hr [] []) <| List.map Aptly.Published.Repository.view model.repositories
