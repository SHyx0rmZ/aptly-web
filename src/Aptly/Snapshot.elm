module Aptly.Snapshot exposing (..)

import Aptly.Generic
import Html
import Html.Attributes
import Html.Events
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

--createCreateRequest : String -> Snapshot -> Http.Request Snapshot
--createCreateRequest server snapshot =
--    Aptly.Generic.httpPost
--        (server ++ "/api/)

createDeleteRequest : Bool -> String -> Snapshot -> Http.Request String
createDeleteRequest force server snapshot =
    Aptly.Generic.httpDelete
        (server ++ "/api/snapshots/" ++ snapshot.name ++ (if force then "?force=1" else ""))
        Http.emptyBody
        Http.expectString

createEditRequest : String -> Snapshot -> Snapshot -> Http.Request Snapshot
createEditRequest server oldSnapshot newSnapshot  =
    Aptly.Generic.httpPut
        (server ++ "/api/snapshots/" ++ oldSnapshot.name)
        (Http.jsonBody <| encodeJson newSnapshot)
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

update : Msg -> Snapshot -> (Snapshot, Cmd Msg)
update msg snapshot =
    case msg of
        NameChanged name ->
            ({ snapshot | name = name }, Cmd.none)

        DescriptionChanged description ->
            ({ snapshot | description = description }, Cmd.none)

view : Maybe (List (String, msg))-> Snapshot -> Html.Html msg
view buttons snapshot =
    Aptly.Generic.viewTable snapshot
        [ ("Name", snapshot.name)
        , ("Description", snapshot.description)
        , ("Created At", snapshot.createdAt)
        ]
        buttons

viewConfirmation : Bool -> (Bool -> msg) -> msg -> (Snapshot -> msg) -> Snapshot -> Html.Html msg
viewConfirmation force forceMsg cancelMsg deleteMsg snapshot =
    Html.div []
        [ Html.p [] [ Html.text <| "Are you sure you want to delete the snapshot \"" ++ snapshot.name ++ "\"?" ]
        , Html.strong [] [ Html.text "Warning!" ]
        , Html.text " This action cannot be undone!"
        , Html.div []
            [ Html.input [ Html.Events.onClick <| forceMsg <| not force, Html.Attributes.type_ "checkbox", Html.Attributes.checked force ] []
            , Html.text "Force"
            ]
        , Html.button [ Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
        , Html.button [ Html.Events.onClick <| deleteMsg snapshot ] [ Html.text "Delete" ]
        ]

viewForm : (Msg -> msg) -> msg -> (Snapshot -> msg) -> Snapshot -> Html.Html msg
viewForm wrapper cancelMsg saveMsg snapshot =
    Aptly.Generic.viewForm snapshot cancelMsg saveMsg wrapper
        [ ("Name", snapshot.name, Just NameChanged)
        , ("Description", snapshot.description, Just DescriptionChanged)
        ]
