module Aptly.SigningOptions exposing (..)

import Json.Decode
import Json.Encode
import Json.Encode.Extra

type alias SigningOptions =
    { skip : Maybe Bool
    , batch : Maybe Bool
    , gpgKey : Maybe String
    , keyring : Maybe String
    , secretKeyring : Maybe String
    , passphrase : Maybe String
    , passphraseFile : Maybe String
    }


decodeJson : Json.Decode.Decoder SigningOptions
decodeJson =
    Json.Decode.map7 SigningOptions
        (Json.Decode.maybe Json.Decode.bool |> Json.Decode.field "Skip")
        (Json.Decode.maybe Json.Decode.bool |> Json.Decode.field "Batch")
        (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "GpgKey")
        (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Keyring")
        (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "SecretKeyring")
        (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "Passphrase")
        (Json.Decode.maybe Json.Decode.string |> Json.Decode.field "PassphraseFile")


encodeJson : SigningOptions -> Json.Encode.Value
encodeJson signingOptions =
    Json.Encode.object
        <| List.filter (\(_, value) -> value /= Json.Encode.null)
            [ ("Skip", Json.Encode.Extra.maybe Json.Encode.bool signingOptions.skip)
            , ("Batch", Json.Encode.Extra.maybe Json.Encode.bool signingOptions.batch)
            , ("GpgKey", Json.Encode.Extra.maybe Json.Encode.string signingOptions.gpgKey)
            , ("Keyring", Json.Encode.Extra.maybe Json.Encode.string signingOptions.keyring)
            , ("SecretKeyring", Json.Encode.Extra.maybe Json.Encode.string signingOptions.secretKeyring)
            , ("Passphrase", Json.Encode.Extra.maybe Json.Encode.string signingOptions.passphrase)
            , ("PassphraseFile", Json.Encode.Extra.maybe Json.Encode.string signingOptions.passphraseFile)
            ]


passphraseFile : String -> SigningOptions
passphraseFile path =
    SigningOptions Nothing (Just True) Nothing Nothing Nothing Nothing (Just path)

skip : SigningOptions
skip =
    SigningOptions (Just True) Nothing Nothing Nothing Nothing Nothing Nothing
