module Aptly.Snapshot exposing (..)

import Aptly.Generic
import Html
import Http
import Json.Decode
import Json.Encode

type alias Timestamp = String

type alias Snapshot =
    { name : String
    , description : String
    , createdAt : Timestamp
    }

type Order
    = Name
    | Time

type Msg
    = NameChanged String
    | DescriptionChanged String

createDeleteRequest : String -> Bool -> Snapshot -> Http.Request String
createDeleteRequest server force snapshot =
    Aptly.Generic.httpDelete
        (server ++ "/api/snapshots/" ++ snapshot.name ++ (if force then "?force=1" else ""))
        Http.emptyBody
        Http.expectString

createEditRequest : String -> String -> Snapshot -> Http.Request Snapshot
createEditRequest server name snapshot =
    Aptly.Generic.httpPut
        (server ++ "/api/snapshots/" ++ name)
        (Http.jsonBody <| encodeJson snapshot)
        (Http.expectJson decodeJson)

createListRequest : String -> Http.Request (List Snapshot)
createListRequest server =
    Http.get (server ++ "/api/snapshots") (Json.Decode.list decodeJson)

decodeJson : Json.Decode.Decoder Snapshot
decodeJson =
    Json.Decode.map3 Snapshot
        (Json.Decode.string |> Json.Decode.field "Name")
        (Json.Decode.string |> Json.Decode.field "Description")
        (Json.Decode.string |> Json.Decode.field "CreatedAt")

encodeJson : Snapshot -> Json.Encode.Value
encodeJson snapshot =
    Json.Encode.object
        [ ("Name", Json.Encode.string snapshot.name)
        , ("Description", Json.Encode.string snapshot.description)
        ]

init : (Snapshot, Cmd msg)
init =
    (Snapshot "" "" "", Cmd.none)

update : Msg -> Maybe Snapshot -> (Maybe Snapshot, Cmd Msg)
update msg maybeSnapshot =
    case maybeSnapshot of
        Nothing ->
            (Nothing, Cmd.none)

        Just snapshot ->
            case msg of
                NameChanged name ->
                    (Just { snapshot | name = name }, Cmd.none)

                DescriptionChanged description ->
                    (Just { snapshot | description = description }, Cmd.none)

view : (Snapshot -> msg) -> (Snapshot -> msg) -> Snapshot -> Html.Html msg
view updateMsg deleteMsg snapshot =
    Aptly.Generic.viewTable snapshot
        [ ("Name", snapshot.name)
        , ("Description", snapshot.description)
        , ("Created At", snapshot.createdAt)
        ]
        <| Just
            [ ("Update", updateMsg snapshot)
            , ("Delete", deleteMsg snapshot)
            ]

viewForm : msg -> (Snapshot -> msg) -> (Msg -> msg) -> Snapshot -> Html.Html msg
viewForm cancelMsg saveMsg wrapper snapshot =
    Aptly.Generic.viewForm snapshot cancelMsg saveMsg wrapper
        [ ("Name", snapshot.name, Just NameChanged)
        , ("Description", snapshot.description, Just DescriptionChanged)
        ]
