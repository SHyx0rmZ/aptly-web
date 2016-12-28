module FilePage exposing (..)

import Aptly.Config
import Aptly.Generic
import Aptly.Generic.List
import Aptly.Generic.SelectableList
import Aptly.Local.Repository
import Aptly.Local.RepositoryListSynchronizer
import Aptly.Upload
import Dict
import Html
import Html.Attributes
import Html.Events
import Http
import Http.Progress
import Json.Decode

type TargetDirectory
    = New Directory
    | Existing Directory

type alias UploadState =
    { directory : TargetDirectory
    , files : List Aptly.Upload.File
    }

type alias AddState =
    { directory : Directory
    , file : Maybe File
    , noRemove : Bool
    , forceReplace : Bool
    }

type State
    = Listing
    | Uploading (Maybe (Http.Progress.Progress String)) UploadState
    | Adding AddState

type alias Model =
    { config : Aptly.Config.Config
    , files : Dict.Dict Directory (List File)
    , state : State
    , repositories : Maybe (Aptly.Generic.SelectableList.SelectableList String)
    }

type Msg
    = Add AddState (Maybe String)
    | AddedResult AddState (Result Http.Error AddResult)
    | Files Directory (Result Http.Error (List File))
    | Directories (Result Http.Error (List Directory))
    | DirectoryChanged String
    | Delete Directory (Maybe File)
    | Deleted Directory (Maybe File) (Result Http.Error String)
    | Radio TargetDirectory Bool
    | RepositoryListModification (Aptly.Generic.List.Modification Aptly.Local.Repository.Repository)
    | SelectRepository String
    | State State
    | ToggleForceReplace Bool
    | ToggleNoRemove Bool
    | UploadProgress Directory Aptly.Upload.FileList (Http.Progress.Progress String)

type alias Directory = String

type alias File = String

type alias AddResult =
    { failedFiles : List String
    , report : AddReport
    }

type alias AddReport =
    { warnings : List String
    , added : List String
    , removed : List String
    }

createAddRequest : String -> AddState -> String -> Http.Request AddResult
createAddRequest server addState repository =
    let
        url =
            case addState.file of
                Nothing ->
                    (server ++ "/api/repos/" ++ repository ++ "/file/" ++ (Http.encodeUri addState.directory))

                Just file ->
                    (server ++ "/api/repos/" ++ repository ++ "/file/" ++ (Http.encodeUri addState.directory) ++ "/" ++ (Http.encodeUri file))

        options =
            List.foldr (\(option, test) list -> if test then list ++ [ option ] else list) [] [ ("forceReplace=1", addState.forceReplace), ("noRemove=1", addState.noRemove) ]
    in
        Aptly.Generic.httpPost
            (url ++ if options /= [] then "?" ++ (String.join "&" options) else "")
            Http.emptyBody
            (Http.expectJson decodeAddResult)

decodeAddResult : Json.Decode.Decoder AddResult
decodeAddResult =
    Json.Decode.map2 AddResult
        (Json.Decode.field "FailedFiles" <| Json.Decode.list Json.Decode.string)
        (Json.Decode.field "Report" decodeAddReport)

decodeAddReport : Json.Decode.Decoder AddReport
decodeAddReport =
    Json.Decode.map3 AddReport
        (Json.Decode.field "Warnings" <| Json.Decode.list Json.Decode.string)
        (Json.Decode.field "Added" <| Json.Decode.list Json.Decode.string)
        (Json.Decode.field "Removed" <| Json.Decode.list Json.Decode.string)

decodeDirectories : Json.Decode.Decoder (List Directory)
decodeDirectories =
    Json.Decode.list Json.Decode.string

decodeFiles : Json.Decode.Decoder (List File)
decodeFiles =
    Json.Decode.list Json.Decode.string

deleteDirectory : String -> Directory -> Http.Request String
deleteDirectory server directory =
    Aptly.Generic.httpDelete
        (server ++ "/api/files/" ++ directory)
        Http.emptyBody
        Http.expectString

deleteFile : String -> Directory -> File -> Http.Request String
deleteFile server directory file =
    Aptly.Generic.httpDelete
        (server ++ "/api/files/" ++ directory ++ "/" ++ file)
        Http.emptyBody
        Http.expectString

getDirectories : String -> Http.Request (List Directory)
getDirectories server =
    Aptly.Generic.httpGet
        (server ++ "/api/files")
        Http.emptyBody
        (Http.expectJson decodeDirectories)

getFiles : String -> Directory -> Http.Request (List File)
getFiles server directory =
    Aptly.Generic.httpGet
        (server ++ "/api/files/" ++ directory)
        Http.emptyBody
        (Http.expectJson decodeFiles)

init : Aptly.Config.Config -> (Model, Cmd Msg)
init config =
        (Model config Dict.empty Listing Nothing, Http.send Directories <| getDirectories config.server)

progressLoaded : Http.Progress.Progress String -> String
progressLoaded progress =
    case progress of
        Http.Progress.Done _ ->
            "100"

        Http.Progress.Some { bytes, bytesExpected } ->
            toString
                <| round
                <| (*) 100
                <| toFloat bytes / toFloat bytesExpected

        _ ->
            "0"

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        <| List.append
            [ Aptly.Local.RepositoryListSynchronizer.onModify RepositoryListModification
            ]
            <| case model.state of
                Uploading (Just _) uploadState ->
                    let
                        directory =
                            case uploadState.directory of
                                New directory ->
                                    directory

                                Existing directory ->
                                    directory
                    in
                        [ Http.Progress.track "upload" (UploadProgress directory uploadState.files) <| Aptly.Upload.request (model.config.server ++ "/api/files/" ++ directory) uploadState.files
                        ]

                _ ->
                    []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add addState Nothing ->
            ({ model | state = Adding addState }, Cmd.none)

        Add addState (Just repository) ->
            (model, Http.send (AddedResult addState) <| createAddRequest model.config.server addState repository)

        AddedResult _ (Err error) ->
            (model, Cmd.none)

        AddedResult addState (Ok result) ->
            let
                files =
                    case addState.noRemove of
                        True ->
                            model.files

                        False ->
                            case addState.file of
                                Nothing ->
                                    Dict.remove addState.directory model.files

                                Just file ->
                                    Dict.update addState.directory (Maybe.map <| List.filter ((/=) file)) model.files
                                        |> Dict.filter (curry (Tuple.second >> (/=) []))

            in
                ({ model | state = Listing, files = files }, Cmd.none)

        Delete directory Nothing ->
            (model, Http.send (Deleted directory Nothing) <| deleteDirectory model.config.server directory)

        Delete directory (Just file) ->
            (model, Http.send (Deleted directory (Just file)) <| deleteFile model.config.server directory file)

        Deleted _ _ (Err _) ->
            (model, Cmd.none)

        Deleted directory Nothing (Ok _) ->
            ({ model | files = Dict.remove directory model.files }, Cmd.none)

        Deleted directory (Just file) (Ok _) ->
            let
                removeFromList maybeList =
                    Maybe.map (List.filter (\item -> item /= file)) maybeList

                files =
                    Dict.update directory removeFromList model.files
            in
                ({ model | files = files }, Cmd.none)

        Directories (Err _) ->
            (model, Cmd.none)

        Directories (Ok directories) ->
            let
                task =
                    directories
                        |> List.map (\directory -> (Files directory, getFiles model.config.server directory))
                        |> List.map (\(callback, request) -> Http.send callback request)

                dictionary =
                    List.foldr (\directory dictionary -> Dict.insert directory [] dictionary) Dict.empty directories
            in
                ({ model | files = dictionary }, Cmd.batch task)

        DirectoryChanged newDirectory ->
            case model.state of
                Uploading Nothing { directory, files } ->
                    case directory of
                        New _ ->
                            ({ model | state = Uploading Nothing <| UploadState (New newDirectory) files }, Cmd.none)

                        _ ->
                            (model, Cmd.none)

                _ ->
                    (model, Cmd.none)

        Files _ (Err _) ->
            (model, Cmd.none)

        Files directory (Ok files) ->
            ({ model | files = Dict.insert directory files model.files }, Cmd.none)

        Radio _ False ->
            (model, Cmd.none)

        Radio targetDirectory True ->
            case model.state of
                Uploading Nothing { files } ->
                    ({ model | state = Uploading Nothing <| UploadState targetDirectory files }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        RepositoryListModification modification ->
            case modification of
                Aptly.Generic.List.List (Ok repositories) ->
                    ({ model | repositories = List.map .name repositories |> Aptly.Generic.SelectableList.selectableList }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        SelectRepository repository ->
            case model.repositories of
                Nothing ->
                    (model, Cmd.none)

                Just repositories ->
                    ({ model | repositories = Aptly.Generic.SelectableList.select repository repositories }, Cmd.none)

        State state ->
            ({ model | state = state }, Cmd.none)

        ToggleForceReplace bool ->
            case model.state of
                Adding addState ->
                    ({ model | state = Adding <| { addState | forceReplace = bool } }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        ToggleNoRemove bool ->
            case model.state of
                Adding addState ->
                    ({ model | state = Adding <| { addState | noRemove = bool } }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        UploadProgress _ _ (Http.Progress.Fail error) ->
            case model.state of
                Uploading (Just _) uploadState ->
                    ({ model | state = Uploading (Just <| Http.Progress.Fail error) uploadState }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        UploadProgress _ _ Http.Progress.None ->
            case model.state of
                Uploading (Just _) uploadState ->
                    ({ model | state = Uploading (Just Http.Progress.None) uploadState }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        UploadProgress _ _ (Http.Progress.Some some) ->
            case model.state of
                Uploading (Just _) uploadState ->
                    ({ model | state = Uploading (Just <| Http.Progress.Some some) uploadState }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        UploadProgress directory files (Http.Progress.Done _) ->
            ({ model | state = Listing, files = List.foldr (\file tree -> Dict.update directory
                (\maybeList ->
                    Just <| case maybeList of
                        Nothing ->
                            [ file.name ]

                        Just files ->
                            List.sort <| files ++ [ file.name ]
                ) tree) model.files files }, Cmd.none)


view : Model -> Html.Html Msg
view model =
    let
        child =
            case model.state of
                Adding addState ->
                    viewAdd model.repositories addState

                Listing ->
                    viewTree model.files

                Uploading maybeProgress uploadState ->
                    viewUpload model.files maybeProgress uploadState
    in
        Html.div []
            [ Html.h1 [] [ Html.text "Files" ]
            , Html.button [ Html.Events.onClick <| State <| Uploading Nothing <| UploadState (New "") [] ] [ Html.text "Upload file" ]
            , Html.hr [] []
            , child
            ]

viewAdd : Maybe (Aptly.Generic.SelectableList.SelectableList String) -> AddState -> Html.Html Msg
viewAdd maybeRepositories addState =
    case maybeRepositories of
        Nothing ->
            Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]

        Just repositories ->
            Html.div []
                [ Html.select [ onSelect SelectRepository ] <| List.map (\item -> Html.option [] [ Html.text item ]) <| Aptly.Generic.SelectableList.items repositories
                , Html.br [] []
                , Html.input [ Html.Events.onClick <| ToggleForceReplace <| not addState.forceReplace, Html.Attributes.type_ "checkbox", Html.Attributes.checked addState.forceReplace ] []
                , Html.text "Force replace"
                , Html.br [] []
                , Html.input [ Html.Events.onClick <| ToggleNoRemove <| not addState.noRemove, Html.Attributes.type_ "checkbox", Html.Attributes.checked addState.noRemove ] []
                , Html.text "No remove"
                , Html.br [] []
                , Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]
                , Html.button [ Html.Events.onClick <| Add addState (Just <| Aptly.Generic.SelectableList.selected repositories) ] [ Html.text "Add to repository" ]
                ]

viewDirectory : Directory -> (List File) -> Html.Html Msg
viewDirectory directory files =
    Html.li []
        [ Html.text directory
        , Html.button [ Html.Events.onClick <| Delete directory Nothing ] [ Html.text "Delete" ]
        , Html.button [ Html.Events.onClick <| Add (AddState directory Nothing False False) Nothing ] [ Html.text "Add to repository" ]
        , Html.ul []
            <| List.map (viewFile directory) files
        ]

viewFile : Directory -> File -> Html.Html Msg
viewFile directory file =
    Html.li []
        [ Html.text file
        , Html.button [ Html.Events.onClick <| Delete directory <| Just file ] [ Html.text "Delete" ]
        , Html.button [ Html.Events.onClick <| Add (AddState directory (Just file) False False) Nothing ] [ Html.text "Add to repository" ]
        ]

viewTree : Dict.Dict Directory (List File) -> Html.Html Msg
viewTree tree =
    Html.ul []
        <| List.map (\(directory, files) -> viewDirectory directory files)
        <| Dict.toList tree


viewUpload : Dict.Dict Directory (List File) -> Maybe (Http.Progress.Progress String) -> UploadState -> Html.Html Msg
viewUpload tree maybeProgress uploadState =
    Html.form []
        [ Html.label []
            [ Html.text "Directory"
--            , Html.input [ Html.Events.onInput (\directory -> State <| Uploading <| UploadState directory uploadState.file), Html.Attributes.value uploadState.directory ] []
            , viewUploadDirectory (Dict.keys tree) uploadState.directory
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "File"
            , Html.input [ onInputs (State << Uploading Nothing << UploadState uploadState.directory), Html.Attributes.type_ "file", Html.Attributes.accept "application/vnd.debian.binary-package", Html.Attributes.multiple True ] []
            ]
        , Html.br [] []
        , Html.button [ Html.Events.onClick <| State Listing, Html.Attributes.type_ "button" ] [ Html.text "Cancel" ]
        , Html.button [ Html.Events.onClick <| State <| Uploading (Just Http.Progress.None) uploadState, Html.Attributes.type_ "button" ] [ Html.text "Upload" ]
        , Html.br [] []
        , viewUploadProgress maybeProgress
        ]

viewUploadDirectory : List Directory -> TargetDirectory -> Html.Html Msg
viewUploadDirectory directories targetDirectory =
    let
        (checked, disabled, value) =
            case targetDirectory of
                New directory ->
                    (True, False, directory)

                Existing directory ->
                    (False, True, "")
    in
        Html.ul [] <|
            (Html.li [] [ Html.input [ Html.Events.onCheck <| Radio <| New "", Html.Attributes.type_ "radio", Html.Attributes.checked checked ] [], Html.input [ Html.Events.onInput <| DirectoryChanged, Html.Attributes.value value, Html.Attributes.disabled disabled ] [] ])
            ::
            (List.map (\directory -> Html.li [] [ Html.input [ Html.Events.onCheck <| Radio <| Existing directory, Html.Attributes.type_ "radio", Html.Attributes.checked (targetDirectory == Existing directory) ] [], Html.text directory ]) directories)

viewUploadProgress : Maybe (Http.Progress.Progress String) -> Html.Html Msg
viewUploadProgress maybeProgress =
    case maybeProgress of
        Nothing ->
            Html.div [] []

        Just progress ->
            Html.div []
                [ Html.span [] [ Html.text "Progress: " ]
                , Html.progress
                    [ Html.Attributes.value <| progressLoaded progress
                    , Html.Attributes.max "100"
                    ]
                    [ Html.text <| (progressLoaded progress) ++ "%"
                    ]
                , Html.text <| (progressLoaded progress) ++ "%"
                , Html.br [] []
                , Html.text "Note: Progress may not be updating correctly"
                ]

onInputs : (Aptly.Upload.FileList -> msg) -> Html.Attribute msg
onInputs tagger =
    Html.Events.on "input"
        <| Json.Decode.map tagger
        <| Json.Decode.map Aptly.Upload.decodeFileList
        <| Json.Decode.at [ "target", "files" ] Json.Decode.value

onSelect : (String -> msg) -> Html.Attribute msg
onSelect tagger =
    Html.Events.on "change" (Json.Decode.map tagger Html.Events.targetValue)
