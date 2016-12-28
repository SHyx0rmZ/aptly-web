module Aptly.Upload exposing (..)

import Json.Decode
import Http
import Native.Aptly.Upload
import Task exposing (Task)

type Error
    = FileReadError

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

request : String -> FileList -> Task Error (Http.Request String)
request url list =
    Native.Aptly.Upload.request url list
