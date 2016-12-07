module Main exposing (..)

import Debug
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
    {
    }

type alias AptlyList
    = List (List String, String)

type Msg
    = PublishList (Result Http.Error AptlyList)

decodeList =
    Json.Decode.list decodePublishedRepository

decodePublishedRepository =
    Json.Decode.map2 (,)
        (Json.Decode.field "Architectures" <| Json.Decode.list Json.Decode.string)
        (Json.Decode.field "Distribution" Json.Decode.string)

init : (Model, Cmd Msg)
init =
    let
        request =
            Http.request
                { method = "GET"
                , headers = []
                , url = "http://127.0.0.1/api/publish"
                , body = Http.emptyBody
                , expect = Http.expectJson decodeList
                , timeout = Nothing
                , withCredentials = False
                }
    in
        (Model, Http.send PublishList request)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PublishList (Err _) ->
            (model, Cmd.none)

        PublishList (Ok response) ->
            let
                _ = Debug.log "response" response
            in
                (model, Cmd.none)

view : Model -> Html.Html Msg
view model =
    Html.h1 [] [ Html.text "Hello world!" ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
