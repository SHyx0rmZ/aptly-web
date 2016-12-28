module Aptly.Upload exposing (..)

import Json.Decode
import Http
import Native.Aptly.Upload

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

request : String -> FileList -> Http.Request String
request url list =
    Native.Aptly.Upload.request url list
