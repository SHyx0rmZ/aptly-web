module Aptly.Package exposing (Package, createShowRequest, view)

import Aptly.Generic
import Html
import Http
import Json.Decode
import Json.Decode.Extra exposing ((|:))

type alias Properties =
    { architecture : Maybe String
    , depends : Maybe String
    , description : Maybe String
    , filename : Maybe String
    , homepage : Maybe String
    , installedSize : Maybe Int
    , md5Sum : Maybe String
    , maintainer : Maybe String
    , package : Maybe String
    , priority : Maybe String
    , sha1 : Maybe String
    , sha256 : Maybe String
    , section : Maybe String
    , size : Maybe Int
    , source : Maybe String
    , version : Maybe String
    }

type alias Package =
    { key : String
    , shortKey : String
    , filesHash : String
    , properties : Properties
    }

createShowRequest : String -> String -> Http.Request Package
createShowRequest server key =
    Http.request
        { method = "GET"
        , headers = []
        , url = server ++ "/api/packages/" ++ key
        , body = Http.emptyBody
        , expect = Http.expectJson decodeJson
        , timeout = Nothing
        , withCredentials = False
        }

decodeJson : Json.Decode.Decoder Package
decodeJson =
    Json.Decode.map4 Package
        (Json.Decode.field "Key" Json.Decode.string)
        (Json.Decode.field "ShortKey" Json.Decode.string)
        (Json.Decode.field "FilesHash" Json.Decode.string)
        decodeJsonProperties

decodeJsonProperties : Json.Decode.Decoder Properties
decodeJsonProperties =
    Json.Decode.succeed Properties
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Architecture")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Depends")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Description")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Filename")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Homepage")
        |: (Json.Decode.maybe Json.Decode.int    |> Json.Decode.field "InstalledSize")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "MD5Sum")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Maintainer")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Package")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Priority")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "SHA1")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "SHA256")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Section")
        |: (Json.Decode.maybe Json.Decode.int    |> Json.Decode.field "Size")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Source")
        |: (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Version")

view : Package -> Html.Html msg
view package =
    Aptly.Generic.viewTable package
        [ ("Key", package.key)
        , ("Short Key", package.shortKey)
        , ("Files Hash", package.filesHash)
        ]
        Nothing
