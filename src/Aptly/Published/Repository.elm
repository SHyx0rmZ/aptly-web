module Aptly.Published.Repository exposing (Msg, Repository, SourceKind(..), createDeleteRequest, createEditRequest, createListRequest, decodeJson, update, view, viewConfirmation, viewForm)

import Aptly.Generic
import Aptly.SigningOptions
import Aptly.Source
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode

type SourceKind
    = Local
    | Snapshot

type alias Repository =
    { storage : String
    , prefix : String
    , distribution : String
    , sourceKind : SourceKind
    , sources : List Aptly.Source.Source
    , architectures : List String
    , label : String
    , origin : String
    , signing : Maybe Aptly.SigningOptions.SigningOptions
    }

type Msg
    = Nothing

createDeleteRequest : Bool -> String -> Repository ->  Http.Request String
createDeleteRequest force server repository =
    Aptly.Generic.httpDelete
        (server ++ "/api/publish/" ++ repository.prefix ++ "/" ++ repository.distribution ++ (if force then "?force=1" else ""))
        Http.emptyBody
        Http.expectString

createEditRequest : String -> Repository -> Repository-> Http.Request Repository
createEditRequest server oldRepository newRepository =
    Aptly.Generic.httpPut
        (server ++ "/api/publish/" ++ oldRepository.prefix ++ "/" ++ oldRepository.distribution)
        (Http.jsonBody <| encodeJson newRepository)
        (Http.expectJson decodeJson)

createListRequest : String -> Http.Request (List Repository)
createListRequest server =
    Http.get (server ++ "/api/publish") (Json.Decode.list decodeJson)

decodeJson : Json.Decode.Decoder Repository
decodeJson =
    Json.Decode.succeed Repository
        |: (Json.Decode.field "Storage" Json.Decode.string)
        |: (Json.Decode.field "Prefix" Json.Decode.string)
        |: (Json.Decode.field "Distribution" Json.Decode.string)
        |: (Json.Decode.field "SourceKind" <| (Json.Decode.string |> Json.Decode.andThen (decodeJsonSourceKind >> Json.Decode.Extra.fromResult)))
        |: (Json.Decode.field "Sources" <| Json.Decode.list Aptly.Source.decodeJson)
        |: (Json.Decode.field "Architectures" <| Json.Decode.list Json.Decode.string)
        |: (Json.Decode.field "Label" Json.Decode.string)
        |: (Json.Decode.field "Origin" Json.Decode.string)
        |: (Json.Decode.maybe <| Json.Decode.field "Signing" Aptly.SigningOptions.decodeJson)

decodeJsonSourceKind : (String -> Result String SourceKind)
decodeJsonSourceKind sourceKind =
    case sourceKind of
        "local" ->
            Ok Local

        "snapshot" ->
            Ok Snapshot

        _ ->
            Err "unknown"

encodeJson : Repository -> Json.Encode.Value
encodeJson repository =
    case repository.sourceKind of
        Local ->
            Json.Encode.object
                [ ("ForceOverwrite", Json.Encode.bool False)
                , ("Signing", Aptly.SigningOptions.encodeJson Aptly.SigningOptions.skip)
                ]

        Snapshot ->
            Json.Encode.object
                [ ("ForceOverwrite", Json.Encode.bool False)
                , ("Snapshots", Json.Encode.list <| List.map Aptly.Source.encodeJson repository.sources)
                , ("Signing", Aptly.SigningOptions.encodeJson Aptly.SigningOptions.skip)
                ]

update : Msg -> Repository -> (Repository, Cmd Msg)
update msg repository =
    case msg of
        Nothing ->
            (repository, Cmd.none)

view : Maybe (List (String, msg)) -> Repository -> Html.Html msg
view buttons repository =
    Aptly.Generic.viewTable repository
        [ ("Storage", repository.storage)
        , ("Prefix", repository.prefix)
        , ("Distribution", repository.distribution)
        , ("Label", repository.label)
        , ("Origin", repository.origin)
        ]
        buttons

viewConfirmation : Bool -> (Bool -> msg) -> msg -> (Repository -> msg) -> Repository -> Html.Html msg
viewConfirmation force forceMsg cancelMsg deleteMsg repository =
    Aptly.Generic.viewConfirmation (Just (force, forceMsg)) cancelMsg (deleteMsg repository) <| "the repository \"" ++ repository.prefix ++ "-" ++ repository.distribution ++ "\""

viewForm : (Msg -> msg) -> msg -> (Repository -> msg) -> Repository -> Html.Html msg
viewForm wrapper cancelMsg updateMsg repository =
    Html.div []
        <| case repository.sourceKind of
            Local ->
                [ Html.text "local"
                , Html.button [ Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
                ]

            Snapshot ->
                [ Html.text "snapshot"
                , Html.hr [] []
                , Html.div [] <| List.map viewFormRow repository.sources
                , Html.hr [] []
                , Html.button [ Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
                , Html.button [ Html.Events.onClick <| updateMsg repository ] [ Html.text "Update" ]
                ]

viewFormRow : Aptly.Source.Source -> Html.Html msg
viewFormRow source =
    Html.table []
        [ Html.tr []
            [ Html.th [ Html.Attributes.align "right" ] [ Html.text "Component" ]
            , Html.td [ Html.Attributes.align "left" ] [ Html.text source.component ]
            ]
        , Html.tr []
            [ Html.th [ Html.Attributes.align "right" ] [ Html.text "Name" ]
            , Html.td [ Html.Attributes.align "left" ]
                [ Html.select []
                    [ Html.option [ Html.Attributes.selected True ] [ Html.text source.name ]
                    ]
                ]
            ]
        ]
