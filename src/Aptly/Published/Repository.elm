module Aptly.Published.Repository exposing (Msg(..), Repository, SourceKind(..), createDeleteRequest, createEditRequest, createListRequest, decodeJson, subscriptions, update, view, viewConfirmation, viewForm)

import Aptly.Generic
import Aptly.SigningOptions
import Aptly.Snapshot
import Aptly.SnapshotList
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
    { snapshotList : Maybe Aptly.SnapshotList.SnapshotList
    , storage : String
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
    = SnapshotListMsg Aptly.SnapshotList.Msg
    | ChangeSource Aptly.Source.Source String

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
    Json.Decode.succeed (Repository Nothing)
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

subscriptions : Repository -> Sub Msg
subscriptions repository =
    case repository.snapshotList of
        Nothing ->
            Sub.none

        Just snapshotList ->
            Sub.map SnapshotListMsg <| Aptly.SnapshotList.subscriptions snapshotList

update : Msg -> Repository -> (Repository, Cmd Msg)
update msg repository =
    case msg of
        ChangeSource source snapshot ->
            ({ repository | sources = Aptly.Generic.replace repository.sources source (Aptly.Source.Source source.component snapshot) }, Cmd.none)

        SnapshotListMsg msg ->
            case repository.snapshotList of
                Nothing ->
                    (repository, Cmd.none)

                Just snapshotList ->
                    let
                        (snapshotListModel, snapshotListMsg) =
                            Aptly.SnapshotList.update msg snapshotList
                    in
                        ({ repository | snapshotList = Just snapshotListModel }, Cmd.map SnapshotListMsg snapshotListMsg)

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
                [ Html.h1 [] [ Html.text <| "Updating " ++ repository.prefix ++ "/" ++ repository.distribution ++ " (" ++ toString repository.sourceKind ++ ")" ]
                , Html.hr [] []
                , Html.button [ Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
                ]

            Snapshot ->
                case repository.snapshotList of
                    Nothing ->
                        [ Html.h1 [] [ Html.text <| "Updating " ++ repository.prefix ++ "/" ++ repository.distribution ++ " (" ++ toString repository.sourceKind ++ ")" ]
                        , Html.hr [] []
                        , Html.div [] [ Html.text "Error" ]
                        , Html.button [ Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
                        ]

                    Just snapshotList ->
                        [ Html.h1 [] [ Html.text <| "Updating " ++ repository.prefix ++ "/" ++ repository.distribution ++ " (" ++ toString repository.sourceKind ++ ")" ]
                        , Html.hr [] []
                        , Html.div [] <| List.map (Html.map wrapper << viewFormRow snapshotList) repository.sources
                        , Html.hr [] []
                        , Html.button [ Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
                        , Html.button [ Html.Events.onClick <| updateMsg repository ] [ Html.text "Update" ]
                        ]

viewFormRow : Aptly.SnapshotList.SnapshotList -> Aptly.Source.Source -> Html.Html Msg
viewFormRow snapshotList source =
    Html.table []
        [ Html.tr []
            [ Html.th [ Html.Attributes.align "right" ] [ Html.text "Component" ]
            , Html.td [ Html.Attributes.align "left" ] [ Html.text source.component ]
            ]
        , Html.tr []
            [ Html.th [ Html.Attributes.align "right" ] [ Html.text "Name" ]
            , Html.td [ Html.Attributes.align "left" ]
                [ Html.select [ onSelect <| ChangeSource source ]
--                    [ Html.option [ Html.Attributes.selected True ] [ Html.text source.name ]
--                    ]
                    <| List.map (viewFormOption source) <| Aptly.SnapshotList.items snapshotList
                ]
            ]
        ]

viewFormOption : Aptly.Source.Source -> Aptly.Snapshot.Snapshot -> Html.Html Msg
viewFormOption source snapshot =
    Html.option [ Html.Attributes.selected <| snapshot.name == source.name ] [ Html.text snapshot.name ]


onSelect : (String -> msg) -> Html.Attribute msg
onSelect tagger =
    Html.Events.on "change" (Json.Decode.map tagger Html.Events.targetValue)
