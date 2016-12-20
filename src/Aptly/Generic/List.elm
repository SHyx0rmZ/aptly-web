module Aptly.Generic.List exposing (..)

import Aptly.Generic
import Html
import Html.Events
import Http

type alias ChangeSet a =
    { old : Maybe a
    , new : Maybe a
    }

type State a
    = Listing
    | Creating a
    | Deleting a
    | Editing a a

type Modification a
    = List (Result Http.Error (List a))
    | Create (Result Http.Error a)
    | Delete a (Result Http.Error String)
    | Edit a (Result Http.Error a)

type Msg a b c
    = State (State a)
    | Modify (Modification a)
    | ItemMsg b
    | Request (State a)
    | ParentMsg c

type alias Model a =
    { items : List a
    , state : State a
    }

type alias CreateRequest a = a -> Http.Request a
type alias DeleteRequest a = a -> Http.Request String
type alias EditRequest a = a -> a -> Http.Request a
type alias ListRequest a = Http.Request (List a)

type alias CancelMsg a b c = Msg a b c
type alias CommitMsg a b c = (a -> Msg a b c)
type alias MsgWrapper a b c = (b -> Msg a b c)
type alias MsgUnwrapper a b c = (Msg a b c -> c)

type alias CreateView a b c = MsgWrapper a b c -> CancelMsg a b c -> CommitMsg a b c -> a -> Html.Html (Msg a b c)
type alias DeleteView a b c = CancelMsg a b c -> CommitMsg a b c -> a -> Html.Html (Msg a b c)
type alias EditView a b c = MsgWrapper a b c -> CancelMsg a b c -> CommitMsg a b c -> a -> Html.Html (Msg a b c)
type alias ListView a b c = Maybe (List (String, Msg a b c)) -> a -> Html.Html (Msg a b c)

type alias RequestFactory a b c =
    { create : Maybe (CreateRequest a, CreateView a b c)
    , delete : Maybe (DeleteRequest a, DeleteView a b c)
    , edit : Maybe (EditRequest a, EditView a b c)
    , list : (ListRequest a, ListView a b c)
    }

mapMsg : c -> Msg a b c
mapMsg msg =
    ParentMsg msg

init : RequestFactory a b c -> (Model a, Cmd (Msg a b c))
init requestFactory =
    (Model [] Listing, Tuple.first requestFactory.list |> Http.send (Modify << List))

update : (b -> a -> (a, Cmd b)) -> RequestFactory a b c -> Msg a b c -> Model a -> (Model a, Cmd (Msg a b c))
update updateItem factory msg model =
    case msg of
        Modify (List (Err _)) ->
            (model, Cmd.none)

        Modify (List (Ok items)) ->
            ({ model | items = items }, Cmd.none)

        State Listing ->
            ({ model | state = Listing }, Cmd.none)

        State (Creating newItem) ->
            ({ model | state = Creating newItem }, Cmd.none)

        State (Deleting oldItem) ->
            ({ model | state = Deleting oldItem }, Cmd.none)

        State (Editing oldItem newItem) ->
            ({ model | state = Editing oldItem newItem }, Cmd.none)

        Modify (Create (Err _)) ->
            (model, Cmd.none)

        Modify (Create (Ok newItem)) ->
            ({ model | state = Listing, items = newItem :: model.items }, Cmd.none)

        Modify (Delete _ (Err _)) ->
            (model, Cmd.none)

        Modify (Delete oldItem (Ok _)) ->
            ({ model | state = Listing, items = List.filter (\item -> item /= oldItem) model.items }, Cmd.none)

        Modify (Edit _ (Err _)) ->
            (model, Cmd.none)

        Modify (Edit oldItem (Ok newItem)) ->
            ({ model | state = Listing, items = Aptly.Generic.replace model.items oldItem newItem }, Cmd.none)

        Request state ->
            case model.state of
                Listing ->
                    (model, Http.send (Modify << List) <| Tuple.first factory.list)

                Creating newItem ->
                    case factory.create of
                        Nothing ->
                            (model, Cmd.none)

                        Just (createRequest, _) ->
                            (model, Http.send (Modify << Create) <| createRequest newItem)

                Deleting oldItem ->
                    case factory.delete of
                        Nothing ->
                            (model, Cmd.none)

                        Just (deleteRequest, _) ->
                            (model, Http.send (Modify << Delete oldItem) <| deleteRequest oldItem)

                Editing oldItem newItem ->
                    case factory.edit of
                        Nothing ->
                            (model, Cmd.none)

                        Just (editRequest, _) ->
                            (model, Http.send (Modify << Edit oldItem) <| editRequest oldItem newItem)

        ItemMsg msg ->
            case model.state of
                Listing ->
                    (model, Cmd.none)

                Creating newItem ->
                    let
                        (itemModel, itemMsg) =
                            updateItem msg newItem
                    in
                        ({ model | state = Creating itemModel }, Cmd.map ItemMsg itemMsg)

                Deleting oldItem ->
                    (model, Cmd.none)

                Editing oldItem newItem ->
                    let
                        (itemModel, itemMsg) =
                            updateItem msg newItem
                    in
                        ({ model | state = Editing oldItem itemModel }, Cmd.map ItemMsg itemMsg)

        ParentMsg msg ->
            (model, Cmd.none)

view : RequestFactory a b c -> a -> String -> Model a -> Html.Html (Msg a b c)
view requestFactory newItem title model =
    Html.div []
        <| case model.state of
            Listing ->
                let
                    (listRequest, listView) =
                        requestFactory.list
                in
                    List.append
                        (List.concat
                            [ [ Html.h1 [] [ Html.text title ] ]
                            , if requestFactory.create /= Nothing then [ Html.button [ Html.Events.onClick <| State <| Creating newItem ] [ Html.text "Create" ] ] else []
                            , [ Html.button [ Html.Events.onClick <| Request Listing ] [ Html.text "Refresh" ] ]
                            , [ Html.hr [] [] ]
                            ])
                        <| List.intersperse (Html.hr [] [])
                            <| List.map (\item -> listView
                                (Just <| List.concat
                                    [ if requestFactory.edit /= Nothing then [ ("Edit", State <| Editing item item) ] else []
                                    , if requestFactory.delete /= Nothing then [ ("Delete", State <| Deleting item) ] else []
                                    ]) item
                                ) model.items

            Creating newItem ->
                case requestFactory.create of
                    Nothing ->
                        []

                    Just (createRequest, createView) ->
                        [ createView ItemMsg (State Listing) (\item -> Request <| Creating item) newItem
                        ]

            Deleting oldItem ->
                case requestFactory.delete of
                    Nothing ->
                        []

                    Just (deleteRequest, deleteView) ->
                        [ deleteView (State Listing) (\item -> Request <| Deleting item) oldItem
                        ]

            Editing oldItem newItem ->
                case requestFactory.edit of
                    Nothing ->
                        []

                    Just (editRequest, editView) ->
                        [ editView ItemMsg (State Listing) (\newItem -> Request <| Editing oldItem newItem) newItem
                        ]
