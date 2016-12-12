module Aptly.Local.Repository exposing (Msg, Repository, decodeJson, createCreateRequest, createDeleteRequest, createEditRequest, createListRequest, init, update, view, viewForm)

import Aptly.Generic
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
    Aptly.Generic.httpPost
        (server ++ "/api/repos")
        (Http.jsonBody <| encodeJson True repository)
        (Http.expectJson decodeJson)

createDeleteRequest : String -> Bool -> Repository ->  Http.Request String
createDeleteRequest server force repository =
    Aptly.Generic.httpDelete
        (server ++ "/api/repos/" ++ repository.name ++ (if force then "?force=1" else ""))
        Http.emptyBody
        Http.expectString

createEditRequest : String -> Repository -> Http.Request Repository
createEditRequest server repository =
    Aptly.Generic.httpPut
        (server ++ "/api/repos/" ++ repository.name)
        (Http.jsonBody <| encodeJson False repository)
        (Http.expectJson decodeJson)

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

update : Msg -> Maybe Repository -> (Maybe Repository, Cmd Msg)
update msg maybeRepository =
    case maybeRepository of
        Nothing ->
            (Nothing, Cmd.none)

        Just repository ->
            case msg of
                NameChanged name ->
                    (Just { repository | name = name }, Cmd.none)

                CommentChanged comment ->
                    (Just { repository | comment = comment }, Cmd.none)

                DefaultDistributionChanged defaultDistribution ->
                    (Just { repository | defaultDistribution = defaultDistribution }, Cmd.none)

                DefaultComponentChanged defaultComponent ->
                    (Just { repository | defaultComponent = defaultComponent }, Cmd.none)


view : (Repository -> msg) -> (Repository -> msg) -> Repository -> Html.Html msg
view editMsg deleteMsg repository =
    Aptly.Generic.viewTable repository
        [ ("Name", repository.name)
        , ("Comment", repository.comment)
        , ("Default Distribution", repository.defaultDistribution)
        , ("Default Component", repository.defaultComponent)
        ]
        <| Just
            [ ("Edit", editMsg repository)
            , ("Delete", deleteMsg repository)
            ]

viewForm : Bool -> (Msg -> msg) -> msg -> (Repository -> msg) -> Repository -> Html.Html msg
viewForm isNew wrapper cancelMsg saveMsg repository =
    Aptly.Generic.viewForm repository cancelMsg saveMsg wrapper
        [ ("Name", repository.name, if isNew then Just NameChanged else Nothing)
        , ("Comment", repository.comment, Just CommentChanged)
        , ("Default Distribution", repository.defaultDistribution, Just DefaultDistributionChanged)
        , ("Default Component", repository.defaultComponent, Just DefaultComponentChanged)
        ]
