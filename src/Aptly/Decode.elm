module Aptly.Decode exposing (..)

import Json.Decode
import Native.Aptly.Decode

decodeOptionsCollection : Json.Decode.Value -> List String
decodeOptionsCollection value =
    Native.Aptly.Decode.decodeOptionsCollection value
