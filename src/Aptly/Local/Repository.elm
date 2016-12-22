module Aptly.Local.Repository exposing (Msg, Repository, decodeJson, createCreateRequest, createDeleteRequest, createEditRequest, createListRequest, init, update, view, viewConfirmation, viewForm)

{-| Represents a local repository in aptly.

# Test
@docs Msg, Repository, decodeJson, createCreateRequest, createDeleteRequest, createEditRequest, createListRequest, init, update, view, viewForm
-}

import Aptly.Generic
import Html
import Http
import Json.Decode
import Json.Encode

{-| Represents a local repository.
-}
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

createDeleteRequest : Bool -> String -> Repository -> Http.Request String
createDeleteRequest force server repository =
    Aptly.Generic.httpDelete
        (server ++ "/api/repos/" ++ repository.name ++ (if force then "?force=1" else ""))
        Http.emptyBody
        Http.expectString

createEditRequest : String -> Repository -> Repository -> Http.Request Repository
createEditRequest server oldRepository newRepository =
    Aptly.Generic.httpPut
        (server ++ "/api/repos/" ++ oldRepository.name)
        (Http.jsonBody <| encodeJson False newRepository)
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
    Json.Encode.object
        <| List.append
            (if includeName then [ ("Name", Json.Encode.string repository.name) ] else [])
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

view : (Repository -> msg) -> Maybe (List (String, msg)) -> Repository -> Html.Html msg
view createSnapshotMsg buttons repository =
    Aptly.Generic.viewTable repository
        [ ("Name", repository.name)
        , ("Comment", repository.comment)
        , ("Default Distribution", repository.defaultDistribution)
        , ("Default Component", repository.defaultComponent)
        ]
        <| Maybe.map (\buttons -> List.append
            buttons
            [ ("Snapshot", createSnapshotMsg repository)
            ])
            buttons

viewConfirmation : Bool -> (Bool -> msg) -> msg -> (Repository -> msg) -> Repository -> Html.Html msg
viewConfirmation force forceMsg cancelMsg deleteMsg repository =
    Aptly.Generic.viewConfirmation (Just (force, forceMsg)) cancelMsg (deleteMsg repository)
        <| "the repository \"" ++ repository.name ++ "\""

viewForm : Bool -> (Msg -> msg) -> msg -> (Repository -> msg) -> Repository -> Html.Html msg
viewForm isNew wrapper cancelMsg saveMsg repository =
    Aptly.Generic.viewForm repository cancelMsg saveMsg wrapper
        [ ("Name", repository.name, if isNew then Just NameChanged else Nothing)
        , ("Comment", repository.comment, Just CommentChanged)
        , ("Default Distribution", repository.defaultDistribution, Just DefaultDistributionChanged)
        , ("Default Component", repository.defaultComponent, Just DefaultComponentChanged)
        ]
