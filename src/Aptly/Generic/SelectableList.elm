module Aptly.Generic.SelectableList exposing (..)

type SelectableList a =
    SelectableList
        { head : List a
        , selected : a
        , tail : List a
        }

isSelect : a -> SelectableList a -> Bool
isSelect item (SelectableList { selected }) =
    item == selected

items : SelectableList a -> List a
items (SelectableList { head, selected, tail }) =
    head ++ [ selected ] ++ tail

member : a -> SelectableList a -> Bool
member item list =
    List.member item <| items list

selectableList : List a -> Maybe (SelectableList a)
selectableList list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            Just
                <| SelectableList
                    { head = []
                    , selected = head
                    , tail = tail
                    }

select : a -> SelectableList a -> Maybe (SelectableList a)
select item list =
    case member item list of
        False ->
            Nothing

        True ->
            let
                (head, selected, tail) = List.foldl
                    (\current (head, selected, tail) ->
                        case selected of
                            Nothing ->
                                if current /= item then
                                    (head ++ [ current ], selected, tail)
                                else
                                    (head, Just current, tail)

                            Just _ ->
                                (head, selected, tail ++ [ current ])
                    )
                    ([], Nothing, [])
                    <| items list
            in
                case selected of
                    Nothing ->
                        Nothing

                    Just selected ->
                        Just
                            <|SelectableList
                                { head = head
                                , selected = selected
                                , tail = tail
                                }

selected : SelectableList a -> a
selected (SelectableList { head, selected,
 tail}) =
    selected
