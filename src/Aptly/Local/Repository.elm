module Aptly.Local.Repository exposing (EditableTarget, Repository, decodeJson, edit, list, update, view, viewForm)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode

type alias Repository =
    { name : String
    , comment : String
    , defaultDistribution : String
    , defaultComponent : String
    }

type EditableTarget
    = Name
    | Comment
    | DefaultDistribution
    | DefaultComponent

type Msg
    = Input EditableTarget String

decodeJson : Json.Decode.Decoder Repository
decodeJson =
    Json.Decode.map4 Repository
        (Json.Decode.field "Name" Json.Decode.string)
        (Json.Decode.field "Comment" Json.Decode.string)
        (Json.Decode.field "DefaultDistribution" Json.Decode.string)
        (Json.Decode.field "DefaultComponent" Json.Decode.string)

edit : String -> Repository -> Http.Request Repository
edit server repository =
    Http.request
        { method = "PUT"
        , headers = []
        , url = (server ++ "/api/repos/" ++ repository.name)
        , body = Http.jsonBody (encodeJson repository)
        , expect = Http.expectJson decodeJson
        , timeout = Nothing
        , withCredentials = False
        }

encodeJson : Repository -> Json.Encode.Value
encodeJson repository =
    Json.Encode.object
        [ ("Comment", Json.Encode.string repository.comment)
        , ("DefaultDistribution", Json.Encode.string repository.defaultDistribution)
        , ("DefaultComponent", Json.Encode.string repository.defaultComponent)
        ]

init : String -> (Repository, Cmd Msg)
init name =
    (Repository name "" "" "", Cmd.none)

list : String -> Http.Request (List Repository)
list server =
    Http.get (server ++ "/api/repos") (Json.Decode.list decodeJson)

update : EditableTarget -> Repository -> String -> Repository
update target repository value =
    case target of
        Name ->
            { repository | name = value }

        Comment ->
            { repository | comment = value }

        DefaultDistribution ->
            { repository | defaultDistribution = value }

        DefaultComponent ->
            { repository | defaultComponent = value }

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

viewForm : Bool -> msg -> (Repository -> msg) -> Repository -> Html.Html msg
viewForm isNew cancelMsg saveMsg repository =
    Html.form []
        [ Html.label []
            [ Html.text "Name"
            , Html.input [ Html.Attributes.value repository.name, Html.Attributes.disabled <| not isNew ] []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Comment"
            , Html.input [ {- Html.Events.onInput <| inputMsg Comment, -} Html.Attributes.value repository.comment ] []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Default Distribution"
            , Html.input [ {- Html.Events.onInput <| inputMsg DefaultDistribution, -} Html.Attributes.value repository.defaultDistribution ] []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Default Component"
            , Html.input [ {- Html.Events.onInput <| inputMsg DefaultComponent, -} Html.Attributes.value repository.defaultComponent ] []
            ]
        , Html.br [] []
        , Html.button [ Html.Attributes.type_ "button", Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
        , Html.button [ Html.Attributes.type_ "button", Html.Events.onClick <| saveMsg repository ] [ Html.text "Save" ]
        ]
