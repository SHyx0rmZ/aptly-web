module Aptly.Source exposing (Source, decodeJson, view)

import Html
import Json.Decode

type alias Source =
    { component : String
    , name : String
    }

decodeJson : Json.Decode.Decoder Source
decodeJson =
    Json.Decode.map2 Source
        (Json.Decode.field "Component" Json.Decode.string)
        (Json.Decode.field "Name" Json.Decode.string)

view : Source -> Html.Html msg
view source =
    Html.span [] [ Html.text source.name ]
