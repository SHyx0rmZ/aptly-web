module Aptly.Published.Repository exposing (Repository, createListRequest, decodeJson, view)

import Aptly.Generic
import Aptly.Source
import Html
import Http
import Json.Decode
import Json.Decode.Extra

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
    }

createListRequest : String -> Http.Request (List Repository)
createListRequest server =
    Http.get (server ++ "/api/publish") (Json.Decode.list decodeJson)

decodeJson : Json.Decode.Decoder Repository
decodeJson =
    Json.Decode.map8 Repository
        (Json.Decode.field "Storage" Json.Decode.string)
        (Json.Decode.field "Prefix" Json.Decode.string)
        (Json.Decode.field "Distribution" Json.Decode.string)
        (Json.Decode.field "SourceKind" <| (Json.Decode.string |> Json.Decode.andThen (decodeJsonSourceKind >> Json.Decode.Extra.fromResult)))
        (Json.Decode.field "Sources" <| Json.Decode.list Aptly.Source.decodeJson)
        (Json.Decode.field "Architectures" <| Json.Decode.list Json.Decode.string)
        (Json.Decode.field "Label" Json.Decode.string)
        (Json.Decode.field "Origin" Json.Decode.string)

decodeJsonSourceKind : (String -> Result String SourceKind)
decodeJsonSourceKind sourceKind =
    case sourceKind of
        "local" ->
            Ok Local

        "snapshot" ->
            Ok Snapshot

        _ ->
            Err "unknown"

view : Repository -> Html.Html msg
view repository =
    Aptly.Generic.viewTable repository
        [ ("Storage", repository.storage)
        , ("Prefix", repository.prefix)
        , ("Distribution", repository.distribution)
        , ("Label", repository.label)
        , ("Origin", repository.origin)
        ]
        Nothing
