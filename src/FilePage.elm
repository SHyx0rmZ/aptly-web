module FilePage exposing (..)

import Aptly.Config
import Aptly.Generic
import Aptly.Upload
import Dict
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Task

type TargetDirectory
    = New Directory
    | Existing Directory

type alias UploadState =
    { directory : TargetDirectory
    , files : List Aptly.Upload.File
    }

type State
    = Listing
    | Uploading UploadState

type alias Model =
    { config : Aptly.Config.Config
    , files : Dict.Dict Directory (List File)
    , state : State
    }

type Msg
    = Files Directory (Result Http.Error (List File))
    | Directories (Result Http.Error (List Directory))
    | DirectoryChanged String
    | Delete Directory (Maybe File)
    | Deleted Directory (Maybe File) (Result Http.Error String)
    | Radio TargetDirectory Bool
    | State State
    | Upload UploadState
    | Uploaded Directory Aptly.Upload.FileList (Result Http.Error String)

type alias Directory = String

type alias File = String

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
        (Model config Dict.empty Listing, Http.send Directories <| getDirectories config.server)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
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
                Uploading { directory, files } ->
                    case directory of
                        New _ ->
                            ({ model | state = Uploading <| UploadState (New newDirectory) files }, Cmd.none)

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
                Uploading { files } ->
                    ({ model | state = Uploading <| UploadState targetDirectory files }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        State state ->
            ({ model | state = state }, Cmd.none)

        Upload uploadState ->
            let
                directory =
                    case uploadState.directory of
                        New directory ->
                            directory

                        Existing directory ->
                            directory
            in
                (model, Http.send (Uploaded directory uploadState.files) <| Aptly.Upload.request (model.config.server ++ "/api/files/" ++ directory) uploadState.files)

        Uploaded _ _ (Err _) ->
            (model, Cmd.none)

        Uploaded directory files (Ok _) ->
            ({ model | state = Listing, files = List.foldr (\file tree -> Dict.update directory
                (\maybeList ->
                    Just <| case maybeList of
                        Nothing ->
                            [ file.name ]

                        Just files ->
                            files ++ [ file.name ]
                ) tree) model.files files }, Cmd.none)


view : Model -> Html.Html Msg
view model =
    let
        child =
            case model.state of
                Listing ->
                    viewTree model.files

                Uploading uploadState ->
                    viewUpload model uploadState
    in
        Html.div []
            [ Html.h1 [] [ Html.text "Files" ]
            , Html.button [ Html.Events.onClick <| State <| Uploading <| UploadState (New "") [] ] [ Html.text "Upload file" ]
            , Html.hr [] []
            , child
            ]

viewDirectory : Directory -> (List File) -> Html.Html Msg
viewDirectory directory files =
    Html.li []
        [ Html.text directory
        , Html.button [ Html.Events.onClick <| Delete directory Nothing ] [ Html.text "Delete" ]
        , Html.ul []
            <| List.map (viewFile directory) files
        ]

viewFile : Directory -> File -> Html.Html Msg
viewFile directory file =
    Html.li []
        [ Html.text file
        , Html.button [ Html.Events.onClick <| Delete directory <| Just file ] [ Html.text "Delete" ]
        ]

viewTree : Dict.Dict Directory (List File) -> Html.Html Msg
viewTree tree =
    Html.ul []
        <| List.map (\(directory, files) -> viewDirectory directory files)
        <| Dict.toList tree


viewUpload : Model -> UploadState -> Html.Html Msg
viewUpload model uploadState =
    Html.form []
        [ Html.label []
            [ Html.text "Directory"
--            , Html.input [ Html.Events.onInput (\directory -> State <| Uploading <| UploadState directory uploadState.file), Html.Attributes.value uploadState.directory ] []
            , viewUploadDirectory (Dict.keys model.files) uploadState.directory
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "File"
            , Html.input [ onInputs (State << Uploading << UploadState uploadState.directory), Html.Attributes.type_ "file", Html.Attributes.accept "application/vnd.debian.binary-package", Html.Attributes.multiple True ] []
            ]
        , Html.br [] []
        , Html.button [ Html.Events.onClick <| State Listing, Html.Attributes.type_ "button" ] [ Html.text "Cancel" ]
        , Html.button [ Html.Events.onClick <| Upload uploadState, Html.Attributes.type_ "button" ] [ Html.text "Upload" ]
        , Html.br [] []
        , Html.progress [ Html.Attributes.max "100", Html.Attributes.value "30" ] []
        ]

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

onInputs : (Aptly.Upload.FileList -> msg) -> Html.Attribute msg
onInputs tagger =
--    Html.Events.on "input" (Json.Decode.map tagger <| Json.Decode.at [ "target", "files" ] <| Json.Decode.list <| Json.Decode.at [ "name" ] Json.Decode.string)
    Html.Events.on "input"
        <| Json.Decode.map tagger
        <| Json.Decode.map Aptly.Upload.decodeFileList
        <| Json.Decode.at [ "target", "files" ] Json.Decode.value

onSelect : (String -> msg) -> Html.Attribute msg
onSelect tagger =
    Html.Events.on "change" (Json.Decode.map tagger Html.Events.targetValue)
