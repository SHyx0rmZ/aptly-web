module Aptly.Config exposing (..)

import Aptly.SigningOptions

type alias Config =
    { server : String
    , signing : Aptly.SigningOptions.SigningOptions
    }
