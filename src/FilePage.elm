module FilePage exposing (..)

import Aptly.Config
import Aptly.Generic
import Dict
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Task

type alias Model =
    { config : Aptly.Config.Config
    , files : Dict.Dict Directory (List File)
    }

type Msg
    = Files Directory (Result Http.Error (List File))
    | Directories (Result Http.Error (List Directory))
    | Delete Directory (Maybe File)
    | Deleted Directory (Maybe File) (Result Http.Error String)

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
        (Model config Dict.empty, Http.send Directories <| getDirectories config.server)

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

        Files _ (Err _) ->
            (model, Cmd.none)

        Files directory (Ok files) ->
            let
                _ = Debug.log directory files
            in
                ({ model | files = Dict.insert directory files model.files }, Cmd.none)

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Files" ]
        , Html.hr [] []
        , viewTree model.files
        ]

viewTree : Dict.Dict Directory (List File) -> Html.Html Msg
viewTree tree =
    Html.ul []
        <| List.map (\(directory, files) -> viewDirectory directory files)
        <| Dict.toList tree

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
