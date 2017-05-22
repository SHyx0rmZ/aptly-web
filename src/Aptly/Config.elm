module Aptly.Config exposing (..)

import Aptly.SigningOptions

type alias Config =
    { server : String
    , signing : Maybe Aptly.SigningOptions.SigningOptions
    }
