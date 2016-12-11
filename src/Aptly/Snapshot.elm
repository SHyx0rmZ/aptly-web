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

createDeleteRequest : String -> Bool -> Snapshot -> Http.Request String
createDeleteRequest server force snapshot =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = server ++ "/api/snapshots/" ++ snapshot.name ++ (if force then "?force=1" else "")
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

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

view : (Snapshot -> msg) -> Snapshot -> Html.Html msg
view deleteMsg snapshot =
    Aptly.Generic.viewTable snapshot
        [ ("Name", snapshot.name)
        , ("Description", snapshot.description)
        , ("Created At", snapshot.createdAt)
        ]
        <| Just
            [ ("Delete", deleteMsg snapshot)
            ]
