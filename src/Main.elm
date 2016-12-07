module Main exposing (..)

import Aptly.Repository
import Aptly.Source
import Html
import Http
import Json.Decode

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

type alias Model =
    { repositories : List Aptly.Repository.Repository
    }

type Msg
    = PublishList (Result Http.Error (List Aptly.Repository.Repository))

decodeList : Json.Decode.Decoder (List Aptly.Repository.Repository)
decodeList =
    Json.Decode.list Aptly.Repository.decodeJson

init : (Model, Cmd Msg)
init =
    let
        request =
            Http.request
                { method = "GET"
                , headers = []
                , url = "http://127.0.0.1:8080/api/publish"
                , body = Http.emptyBody
                , expect = Http.expectJson decodeList
                , timeout = Nothing
                , withCredentials = False
                }
    in
        (Model [], Http.send PublishList request)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PublishList (Err _) ->
            (model, Cmd.none)

        PublishList (Ok repositories) ->
            ({ model | repositories = repositories }, Cmd.none)

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Published repositories" ]
        , Html.ul [] <| List.map Aptly.Repository.view model.repositories
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
