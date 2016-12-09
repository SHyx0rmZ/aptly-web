module PackagePage exposing (..)

import Aptly.Package
import Html

type Msg
    = List
    | Edit Aptly.Package.Package
    | Delete Aptly.Package.Package

type alias Model =
    { packages : List Aptly.Package.Package
    , server : String
    }

init : String -> (Model, Cmd Msg)
init server =
    (Model [] server, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        List ->
            (model, Cmd.none)

        Edit package ->
            (model, Cmd.none)

        Delete package ->
            (model, Cmd.none)

view : Model -> Html.Html Msg
view model =
    Html.div []
        <| List.append
            [ Html.h1 [] [ Html.text "Packages" ]
            ]
            <| List.map Aptly.Package.view model.packages
