module Aptly.Upload exposing (..)

import Json.Decode
import Http
import Native.Aptly.Upload
import Task exposing (Task)

type alias FileList = List File

type alias File =
    { name : String
    , size : Int
    , mime : String
    , object : Json.Decode.Value
    }

decodeFileList : Json.Decode.Value -> FileList
decodeFileList value =
    Native.Aptly.Upload.decodeFileList value

request : String -> File -> Http.Request String
request url file =
    let
        _ = Debug.log "fff" file
    in
        Native.Aptly.Upload.request url file
