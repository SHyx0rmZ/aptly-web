module Aptly.Published.Repository exposing (Msg(..), Repository, SourceKind(..), createCreateRequest, createDeleteRequest, createEditRequest, createListRequest, decodeJson, subscriptions, update, view, viewConfirmation, viewCreate, viewForm)

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
    | PrefixChanged String
    | DistributionChanged String
    | LabelChanged String
    | OriginChanged String
    | ArchitecturesChanged (List String)
    | SourcesChanged (List Aptly.Source.Source)

createCreateRequest : String -> Repository -> Http.Request Repository
createCreateRequest server repository =
    Aptly.Generic.httpPost
        (server ++ "/api/publish/" ++ repository.prefix)
        (Http.jsonBody <| encodeJsonCreate repository)
        (Http.expectJson decodeJson)

createDeleteRequest : Bool -> String -> Repository -> Http.Request String
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

encodeJsonCreate : Repository -> Json.Encode.Value
encodeJsonCreate repository =
    case repository.sourceKind of
        Local ->
            Json.Encode.object
                []

        Snapshot ->
            Json.Encode.object
                [ ("SourceKind", Json.Encode.string <| String.toLower <| toString repository.sourceKind)
                , ("Sources", Json.Encode.list <| List.map Aptly.Source.encodeJson repository.sources)
                , ("Distribution", Json.Encode.string repository.distribution)
                , ("Label", Json.Encode.string repository.label)
                , ("Origin", Json.Encode.string repository.origin)
                , ("ForceOverwrite", Json.Encode.bool False)
                , ("Architectures", Json.Encode.list <| List.map Json.Encode.string repository.architectures)
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

        PrefixChanged prefix ->
            ({ repository | prefix = prefix }, Cmd.none)

        DistributionChanged distribution ->
            ({ repository | distribution = distribution }, Cmd.none)

        LabelChanged label ->
            ({ repository | label = label }, Cmd.none)

        OriginChanged origin ->
            ({ repository | origin = origin }, Cmd.none)

        ArchitecturesChanged architectures ->
            ({ repository | architectures = architectures }, Cmd.batch [])

        SourcesChanged sources ->
            ({ repository | sources = sources }, Cmd.none)

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

viewCreate : String -> Aptly.Source.Source -> (String -> msg) -> (Aptly.Source.Source -> msg) -> (Msg -> msg) -> msg -> (Repository -> msg) -> Repository -> Html.Html msg
viewCreate newArchitecture newSource newArchitectureMsg newSourceMsg wrapper cancelMsg createMsg repository =
    Html.div []
        [ Html.fieldset []
            [ Html.legend [] [ Html.text "Source kind" ]
            , Html.label []
                [ Html.input [ Html.Attributes.type_ "radio", Html.Attributes.checked True ] []
                , Html.text "Snapshot"
                ]
            , Html.br [] []
            , Html.label []
                [ Html.input [ Html.Attributes.type_ "radio", Html.Attributes.checked False, Html.Attributes.disabled True ] []
                , Html.text "Local repository"
                ]
            ]
        , Html.br [] []
        , Html.form []
            [ Html.label []
                [ Html.text "Prefix"
                , Html.input [ Html.Events.onInput <| wrapper << PrefixChanged, Html.Attributes.value repository.prefix ] []
                ]
            , Html.br [] []
            , Html.label []
                [ Html.text "Distribution"
                , Html.input [ Html.Events.onInput <| wrapper << DistributionChanged, Html.Attributes.value repository.distribution ] []
                ]
            , Html.br [] []
            , Html.label []
                [ Html.text "Label"
                , Html.input [ Html.Events.onInput <| wrapper << LabelChanged, Html.Attributes.value repository.label ] []
                ]
            , Html.br [] []
            , Html.label []
                [ Html.text "Origin"
                , Html.input [ Html.Events.onInput <| wrapper << OriginChanged, Html.Attributes.value repository.origin ] []
                ]
            , Html.br [] []
            , Html.label []
                [ Html.text "Force Overwrite"
                , Html.input [ Html.Attributes.type_ "checkbox", Html.Attributes.checked False, Html.Attributes.disabled True ] []
                ]
            , Html.br [] []
            , Html.fieldset []
                [ Html.legend [] [ Html.text "Architectures" ]
                , Html.fieldset []
                    [ Html.legend [] [ Html.text "New architecture" ]
                    , Html.input [ Html.Events.onInput newArchitectureMsg, Html.Attributes.value newArchitecture ] []
                    , Html.br [] []
                    , Html.button [ Html.Events.onClick <| wrapper <| ArchitecturesChanged <| repository.architectures ++ [ newArchitecture ], Html.Attributes.type_ "button", Html.Attributes.disabled <| newArchitecture == "" || List.member newArchitecture repository.architectures ] [ Html.text "Add architecture" ]
                    ]
                , Html.div [] <| List.map (Html.map wrapper << viewCreateArchitecture repository.architectures) repository.architectures
                ]
            , Html.br [] []
            , Html.fieldset []
                [ Html.legend [] [ Html.text "Sources" ]
                , viewCreateNewSource newSource newSourceMsg wrapper repository
                , Html.div [] <| List.map (Html.map wrapper << viewCreateSource repository.sources) repository.sources
                ]
            , Html.br [] []
            , Html.button [ Html.Events.onClick <| cancelMsg, Html.Attributes.type_ "button"  ] [ Html.text "Cancel" ]
            , Html.button [ Html.Events.onClick <| createMsg repository, Html.Attributes.type_ "button", Html.Attributes.disabled <| 0 == List.length repository.sources ] [ Html.text "Create" ]
            ]
        ]

viewCreateArchitecture : List String -> String -> Html.Html Msg
viewCreateArchitecture architectures architecture =
    Html.div []
        [ Html.text architecture
        , Html.button [ Html.Events.onClick <| ArchitecturesChanged (List.filter ((/=) architecture) architectures), Html.Attributes.type_ "button" ] [ Html.text "Remove architecture" ]
        ]

viewCreateNewSource : Aptly.Source.Source -> (Aptly.Source.Source -> msg) -> (Msg -> msg) -> Repository -> Html.Html msg
viewCreateNewSource newSource newSourceMsg wrapper repository =
    case repository.snapshotList of
        Nothing ->
            Html.text "Loading snapshots..."

        Just snapshotList ->
            case Aptly.SnapshotList.items snapshotList of
                [] ->
                    Html.text "No snapshots available."

                snapshots ->
                    Html.fieldset []
                        [ Html.legend [] [ Html.text "New source" ]
                        , Html.label []
                            [ Html.text "Component"
                            , Html.input [ Html.Events.onInput (\component -> newSourceMsg <| Aptly.Source.Source component newSource.name), Html.Attributes.value newSource.component ] []
                            ]
                        , Html.br [] []
                        , Html.label []
                            [ Html.text "Name"
                            , Html.select [ onSelect <| (\name -> newSourceMsg <| Aptly.Source.Source newSource.component name) ] <| List.map (\snapshot -> Html.option [ Html.Attributes.selected <| snapshot.name == newSource.name ] [ Html.text snapshot.name ]) snapshots
                            ]
                        , Html.br [] []
                        , Html.button [ Html.Events.onClick <| wrapper <| SourcesChanged <| repository.sources ++ [ Aptly.Source.Source newSource.component newSource.name ], Html.Attributes.type_ "button", Html.Attributes.disabled <| newSource.component == "" || (List.member newSource.component <| List.map .component repository.sources) ] [ Html.text "Add source" ]
                        ]

viewCreateSource : List Aptly.Source.Source -> Aptly.Source.Source -> Html.Html Msg
viewCreateSource sources source =
    Html.div []
        [ Html.table []
            [ Html.tr []
                [ Html.th [] [ Html.text "Component" ]
                , Html.td [] [ Html.text source.component ]
                ]
            , Html.tr []
                [ Html.th [] [ Html.text "Name" ]
                , Html.td [] [ Html.text source.name ]
                ]
            ]
        , Html.button [ Html.Events.onClick <| SourcesChanged (List.filter ((/=) source) sources), Html.Attributes.type_ "button" ] [ Html.text "Remove source" ]
        ]

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
