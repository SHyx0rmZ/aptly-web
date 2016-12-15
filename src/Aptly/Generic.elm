module Aptly.Generic exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http

http : String -> String -> Http.Body -> Http.Expect a -> Http.Request a
http method url body expect =
    Http.request
        { method = method
        , headers = []
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }

httpDelete : String -> Http.Body -> Http.Expect a -> Http.Request a
httpDelete =
    http "DELETE"

httpGet : String -> Http.Body -> Http.Expect a -> Http.Request a
httpGet =
    http "GET"

httpPost : String -> Http.Body -> Http.Expect a -> Http.Request a
httpPost =
    http "POST"

httpPut : String -> Http.Body -> Http.Expect a -> Http.Request a
httpPut =
    http "PUT"

replace : List a -> a -> a -> List a
replace list old new =
    List.map (\item ->
        if item == old then
            new
        else
            item) list

{-| Renders a table. -}
viewTable : a -> List (String, String) -> Maybe (List (String, msg)) -> Html.Html msg
viewTable model properties maybeButtons =
    Html.table []
        <| List.append
            (List.map viewTableRow properties)
            <| case maybeButtons of
                Nothing ->
                    []

                Just buttons ->
                    [ Html.tr []
                        <| List.map (\(label, action) -> Html.button [ Html.Events.onClick action ] [ Html.text label ]) buttons
                    ]

viewTableRow : (String, String) -> Html.Html msg
viewTableRow (label, value) =
    Html.tr []
        [ Html.th [ Html.Attributes.align "right" ] [ Html.text label ]
        , Html.td [ Html.Attributes.align "left" ] [ Html.text value ]
        ]

viewConfirmation : Maybe (Bool, Bool -> msg) -> msg -> msg -> String -> Html.Html msg
viewConfirmation maybeForce cancelMsg deleteMsg name =
    let
        forceDiv =
            case maybeForce of
                Nothing ->
                    []

                Just (force, forceMsg) ->
                    [ Html.input [ Html.Events.onClick <| forceMsg <| not force, Html.Attributes.type_ "checkbox", Html.Attributes.checked force ] []
                    , Html.text "Force"
                    ]
    in
        Html.div []
            [ Html.p [] [ Html.text <| "Are you sure you want to delete " ++ name ++ "?" ]
            , Html.strong [] [ Html.text "Warning!" ]
            , Html.text " This action cannot be undone!"
            , Html.div [] forceDiv
            , Html.button [ Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
            , Html.button [ Html.Events.onClick <| deleteMsg ] [ Html.text "Delete" ]
            ]

viewForm : a -> msgB -> (a -> msgB) -> (msgA -> msgB) -> List (String, String, Maybe (String -> msgA)) -> Html.Html msgB
viewForm model cancelMsg saveMsg wrapper properties =
    Html.form []
        <| List.append
            (List.map (viewFormRow wrapper) properties |> (List.intersperse <| Html.br [] []))
            [ Html.br [] []
            , Html.button [ Html.Attributes.type_ "button", Html.Events.onClick <| cancelMsg ] [ Html.text "Cancel" ]
            , Html.button [ Html.Attributes.type_ "button", Html.Events.onClick <| saveMsg model ] [ Html.text "Save" ]
            ]

viewFormRow : (msgA -> msgB) -> (String, String, Maybe (String -> msgA)) -> Html.Html msgB
viewFormRow wrapper (label, value, maybeMsg) =
    Html.label []
        [ Html.text label
        , case maybeMsg of
            Nothing ->
                Html.input [ Html.Attributes.disabled True, Html.Attributes.value value ] []

            Just msg ->
                Html.input [ Html.Events.onInput <| (\value -> wrapper <| msg value), Html.Attributes.value value ] []
        ]
