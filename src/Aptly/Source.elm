module Aptly.Source exposing (Source, decodeJson, encodeJson, view)

import Html
import Json.Decode
import Json.Encode

type alias Source =
    { component : String
    , name : String
    }

decodeJson : Json.Decode.Decoder Source
decodeJson =
    Json.Decode.map2 Source
        (Json.Decode.field "Component" Json.Decode.string)
        (Json.Decode.field "Name" Json.Decode.string)

encodeJson : Source -> Json.Encode.Value
encodeJson source =
    Json.Encode.object
        [ ("Component", Json.Encode.string source.component)
        , ("Name", Json.Encode.string source.name)
        ]

view : Source -> Html.Html msg
view source =
    Html.span [] [ Html.text source.name ]
