module Aptly.Published.Repository exposing (Repository, decodeJson, view)

import Aptly.Source
import Html
import Json.Decode

type alias Repository =
    { storage : String
    , prefix : String
    , distribution : String
    , sourceKind : String
    , sources : List Aptly.Source.Source
    , architectures : List String
    , label : String
    , origin : String
    }

decodeJson : Json.Decode.Decoder Repository
decodeJson =
    Json.Decode.map8 Repository
        (Json.Decode.field "Storage" Json.Decode.string)
        (Json.Decode.field "Prefix" Json.Decode.string)
        (Json.Decode.field "Distribution" Json.Decode.string)
        (Json.Decode.field "SourceKind" Json.Decode.string)
        (Json.Decode.field "Sources" <| Json.Decode.list Aptly.Source.decodeJson)
        (Json.Decode.field "Architectures" <| Json.Decode.list Json.Decode.string)
        (Json.Decode.field "Label" Json.Decode.string)
        (Json.Decode.field "Origin" Json.Decode.string)

view : Repository -> Html.Html msg
view repo =
    Html.li []
        [ Html.div []
            <| List.append
                [ Html.text (repo.prefix ++ "/" ++ repo.distribution)
                , Html.br [] []
                ]
                (List.map Aptly.Source.view repo.sources)
        ]
