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
    { repositories : List AptlyRepository
    }

type alias AptlySource =
    { component : String
    , name : String
    }

type alias AptlyRepository =
    { storage : String
    , prefix : String
    , distribution : String
    , sourceKind : String
    , sources : List AptlySource
    , architectures : List String
    , label : String
    , origin : String
    }

type Msg
    = PublishList (Result Http.Error (List AptlyRepository))

decodeList : Json.Decode.Decoder (List AptlyRepository)
decodeList =
    Json.Decode.list decodePublishedRepository

decodeSource : Json.Decode.Decoder AptlySource
decodeSource =
    Json.Decode.map2 AptlySource
        (Json.Decode.field "Component" Json.Decode.string)
        (Json.Decode.field "Name" Json.Decode.string)

decodePublishedRepository : Json.Decode.Decoder AptlyRepository
decodePublishedRepository =
    Json.Decode.map8 AptlyRepository
        (Json.Decode.field "Storage" Json.Decode.string)
        (Json.Decode.field "Prefix" Json.Decode.string)
        (Json.Decode.field "Distribution" Json.Decode.string)
        (Json.Decode.field "SourceKind" Json.Decode.string)
        (Json.Decode.field "Sources" <| Json.Decode.list decodeSource)
        (Json.Decode.field "Architectures" <| Json.Decode.list Json.Decode.string)
        (Json.Decode.field "Label" Json.Decode.string)
        (Json.Decode.field "Origin" Json.Decode.string)

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
        , Html.ul [] <| List.map viewRepository model.repositories
        ]

viewRepository : AptlyRepository -> Html.Html msg
viewRepository repo =
    Html.li []
        [ Html.div []
            <| List.append
                [ Html.text (repo.prefix ++ "/" ++ repo.distribution)
                , Html.br [] []
                ]
                (List.map viewSource repo.sources)
        ]

viewSource : AptlySource -> Html.Html msg
viewSource source =
    Html.span [] [ Html.text source.name ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
