module Aptly.Local.Repository exposing (Msg, Repository, decodeJson, createCreateRequest, createEditRequest, createListRequest, init, update, view, viewForm)

import Debug
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

type Msg
    = NameChanged String
    | CommentChanged String
    | DefaultDistributionChanged String
    | DefaultComponentChanged String

createCreateRequest : String -> Repository -> Http.Request Repository
createCreateRequest server repository =
    Http.request
        { method = "POST"
        , headers = []
        , url = server ++ "/api/repos"
        , body = Http.jsonBody <| encodeJson True repository
        , expect = Http.expectJson decodeJson
        , timeout = Nothing
        , withCredentials = False
        }

createEditRequest : String -> Repository -> Http.Request Repository
createEditRequest server repository =
    Http.request
        { method = "PUT"
        , headers = []
        , url = server ++ "/api/repos/" ++ repository.name
        , body = Http.jsonBody <| encodeJson False repository
        , expect = Http.expectJson decodeJson
        , timeout = Nothing
        , withCredentials = False
        }

createListRequest : String -> Http.Request (List Repository)
createListRequest server =
    Http.get (server ++ "/api/repos") (Json.Decode.list decodeJson)

decodeJson : Json.Decode.Decoder Repository
decodeJson =
    Json.Decode.map4 Repository
        (Json.Decode.field "Name" Json.Decode.string)
        (Json.Decode.field "Comment" Json.Decode.string)
        (Json.Decode.field "DefaultDistribution" Json.Decode.string)
        (Json.Decode.field "DefaultComponent" Json.Decode.string)

encodeJson : Bool -> Repository -> Json.Encode.Value
encodeJson includeName repository =
    let
        maybeName =
            case includeName of
                True ->
                    [ ("Name", Json.Encode.string repository.name) ]

                False ->
                    []
    in
        {- aptly currently does not support updating with empty strings and will just ignore them -}
        Json.Encode.object
            <| List.append
                maybeName
                [ ("Comment", Json.Encode.string repository.comment)
                , ("DefaultDistribution", Json.Encode.string repository.defaultDistribution)
                , ("DefaultComponent", Json.Encode.string repository.defaultComponent)
                ]

init : String -> (Repository, Cmd Msg)
init name =
    (Repository name "" "" "", Cmd.none)

update : Msg -> Repository -> (Repository, Cmd Msg)
update msg repository =
    case msg of
        NameChanged name ->
            ({ repository | name = name }, Cmd.none)

        CommentChanged comment ->
            ({ repository | comment = comment }, Cmd.none)

        DefaultDistributionChanged defaultDistribution ->
            ({ repository | defaultDistribution = defaultDistribution }, Cmd.none)

        DefaultComponentChanged defaultComponent ->
            ({ repository | defaultComponent = defaultComponent }, Cmd.none)


view : (Repository -> msg) -> Repository -> Html.Html msg
view editMsg repository =
    viewTable repository editMsg
        [ ("Name", repository.name)
        , ("Comment", repository.comment)
        , ("Default Distribution", repository.defaultDistribution)
        , ("Default Component", repository.defaultComponent)
        ]

viewTable : a -> (a -> msg) -> List (String, String) -> Html.Html msg
viewTable model editMsg properties =
    Html.table []
        <| List.append
            (List.map viewTableRow properties)
            [ Html.tr []
                [ Html.button [ Html.Events.onClick <| editMsg model ] [ Html.text "Edit" ]
                ]
            ]

viewTableRow : (String, String) -> Html.Html msg
viewTableRow (label, value) =
    Html.tr []
        [ Html.th [ Html.Attributes.align "right" ] [ Html.text label ]
        , Html.td [ Html.Attributes.align "left" ] [ Html.text value ]
        ]

viewFormGeneric : a -> msgB -> (a -> msgB) -> (msgA -> msgB) -> List (String, String, Maybe (String -> msgA)) -> Html.Html msgB
viewFormGeneric model cancelMsg saveMsg wrapper properties =
    Html.form []
        <| List.append
            (List.map (viewFormRow wrapper) properties |> (List.intersperse <| Html.br [] []))
            [ Html.br [] []
            , Html.button [ Html.Attributes.type_ "button", Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
            , Html.button [ Html.Attributes.type_ "button", Html.Events.onClick <| saveMsg model ] [ Html.text "Save" ]
            ]

viewFormRow : (msgA -> msgB) -> (String, String, Maybe (String -> msgA)) -> Html.Html msgB
viewFormRow wrapper (label, value, maybeMsg) =
    Html.label []
        [ Html.text label
        , case maybeMsg of
            Nothing ->
                Html.input [ Html.Attributes.disabled True, Html.Attributes.value value ] []

            Just msg ->
                Html.input [ Html.Events.onInput <| (\value -> wrapper <| msg value), Html.Attributes.value value ] []
        ]

viewForm : Bool -> (Msg -> msg) -> msg -> (Repository -> msg) -> Repository -> Html.Html msg
viewForm isNew wrapper cancelMsg saveMsg repository =
    let
        nameAction =
            case isNew of
                True ->
                    Just NameChanged

                False ->
                    Nothing
    in
        viewFormGeneric repository cancelMsg saveMsg wrapper
            [ ("Name", repository.name, nameAction)
            , ("Comment", repository.comment, Just CommentChanged)
            , ("Default Distribution", repository.defaultDistribution, Just DefaultDistributionChanged)
            , ("Default Component", repository.defaultComponent, Just DefaultComponentChanged)
            ]
