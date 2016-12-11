module Aptly.Snapshot exposing (..)

import Aptly.Generic
import Html
import Http
import Json.Decode

type alias Timestamp = String

type alias Snapshot =
    { name : String
    , description : String
    , createdAt : Timestamp
    }

type Order
    = Name
    | Time

createListRequest : String -> Http.Request (List Snapshot)
createListRequest server =
    Http.get (server ++ "/api/snapshots") (Json.Decode.list decodeJson)

decodeJson : Json.Decode.Decoder Snapshot
decodeJson =
    Json.Decode.map3 Snapshot
        (Json.Decode.string |> Json.Decode.field "Name")
        (Json.Decode.string |> Json.Decode.field "Description")
        (Json.Decode.string |> Json.Decode.field "CreatedAt")

init : (Snapshot, Cmd msg)
init =
    (Snapshot "" "" "", Cmd.none)

view : Snapshot -> Html.Html msg
view snapshot =
    Aptly.Generic.viewTable snapshot
        [ ("Name", snapshot.name)
        , ("Description", snapshot.description)
        , ("Created At", snapshot.createdAt)
        ]
        Nothing
