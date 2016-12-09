module Aptly.Generic exposing (..)

import Html
import Html.Attributes
import Html.Events

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
