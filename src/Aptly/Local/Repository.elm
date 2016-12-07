module Aptly.Local.Repository exposing (Repository, decodeJson, list, view, viewForm)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode

type alias Repository =
    { name : String
    , comment : String
    , defaultDistribution : String
    , defaultComponent : String
    }

decodeJson : Json.Decode.Decoder Repository
decodeJson =
    Json.Decode.map4 Repository
        (Json.Decode.field "Name" Json.Decode.string)
        (Json.Decode.field "Comment" Json.Decode.string)
        (Json.Decode.field "DefaultDistribution" Json.Decode.string)
        (Json.Decode.field "DefaultComponent" Json.Decode.string)

list : String -> Http.Request (List Repository)
list server =
    Http.get (server ++ "/api/repos") (Json.Decode.list decodeJson)

view : (Repository -> msg) -> Repository -> Html.Html msg
view editMsg repository =
    Html.table []
        [ Html.tr []
            [ Html.th [ Html.Attributes.align "right" ] [ Html.text "Name" ]
            , Html.td [ Html.Attributes.align "left" ] [ Html.text repository.name ]
            ]
        , Html.tr []
            [ Html.th [ Html.Attributes.align "right" ] [ Html.text "Comment" ]
            , Html.td [ Html.Attributes.align "left" ] [ Html.text repository.comment ]
            ]
        , Html.tr []
            [ Html.th [ Html.Attributes.align "right" ] [ Html.text "Default Distribution" ]
            , Html.td [ Html.Attributes.align "left" ] [ Html.text repository.defaultDistribution ]
            ]
        , Html.tr []
            [ Html.th [ Html.Attributes.align "right" ] [ Html.text "Default Component" ]
            , Html.td [ Html.Attributes.align "left" ] [ Html.text repository.defaultComponent ]
            ]
        , Html.tr []
            [ Html.button [ Html.Events.onClick <| editMsg repository ] [ Html.text "Edit" ]
            ]
        ]

viewForm : Bool -> Repository -> Html.Html msg
viewForm isNew repository =
    Html.form []
        [ Html.label []
            [ Html.text "Name"
            , Html.input [ Html.Attributes.value repository.name, Html.Attributes.disabled <| not isNew ] []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Comment"
            , Html.input [ Html.Attributes.value repository.comment ] []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Default Distribution"
            , Html.input [ Html.Attributes.value repository.defaultDistribution ] []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Default Component"
            , Html.input [ Html.Attributes.value repository.defaultComponent ] []
            ]
        , Html.br [] []
        , Html.button [ {- Html.Events.onClick <| cancelMsg -} ] [ Html.text "Cancel" ]
        , Html.button [ {- Html.Events.onClick <| saveMsg repository -} ] [ Html.text "Save" ]
        ]
