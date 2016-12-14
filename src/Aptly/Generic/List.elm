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

type Msg a b
    = List (Result Http.Error (List a))
    | State (State a)
    | Create (Result Http.Error a)
    | Delete a (Result Http.Error String)
    | Edit a (Result Http.Error a)
    | ItemMsg b
    | Request (State a)

type alias Model a =
    { items : List a
    , state : State a
    }

type alias CreateRequest a = a -> Http.Request a
type alias DeleteRequest a = a -> Http.Request String
type alias EditRequest a = a -> a -> Http.Request a
type alias ListRequest a = Http.Request (List a)

type alias CancelMsg a b = Msg a b
type alias CommitMsg a b = (a -> Msg a b)
type alias MsgWrapper a b = (b -> Msg a b)

type alias CreateView a b = MsgWrapper a b -> CancelMsg a b -> CommitMsg a b -> a -> Html.Html (Msg a b)
type alias DeleteView a b = CancelMsg a b -> CommitMsg a b -> a -> Html.Html (Msg a b)
type alias EditView a b = MsgWrapper a b -> CancelMsg a b -> CommitMsg a b -> a -> Html.Html (Msg a b)
type alias ListView a b = Maybe (List (String, Msg a b)) -> a -> Html.Html (Msg a b)

type alias RequestFactory a b =
    { create : Maybe (CreateRequest a, CreateView a b)
    , delete : Maybe (DeleteRequest a, DeleteView a b)
    , edit : Maybe (EditRequest a, EditView a b)
    , list : (ListRequest a, ListView a b)
    }

init : RequestFactory a b -> (Model a, Cmd (Msg a b))
init requestFactory =
    (Model [] Listing, Tuple.first requestFactory.list |> Http.send List)

update : (aMsg -> a -> (a, Cmd aMsg)) -> RequestFactory a b -> Msg a aMsg -> Model a -> (Model a, Cmd (Msg a aMsg))
update updateItem factory msg model =
    case msg of
        List (Err _) ->
            (model, Cmd.none)

        List (Ok items) ->
            ({ model | items = items }, Cmd.none)

        State Listing ->
            ({ model | state = Listing }, Cmd.none)

        State (Creating newItem) ->
            ({ model | state = Creating newItem }, Cmd.none)

        State (Deleting oldItem) ->
            ({ model | state = Deleting oldItem }, Cmd.none)

        State (Editing oldItem newItem) ->
            ({ model | state = Editing oldItem newItem }, Cmd.none)

        Create (Err _) ->
            (model, Cmd.none)

        Create (Ok newItem) ->
            ({ model | state = Listing, items = newItem :: model.items }, Cmd.none)

        Delete _ (Err _) ->
            (model, Cmd.none)

        Delete oldItem (Ok _) ->
            ({ model | state = Listing, items = List.filter (\item -> item /= oldItem) model.items }, Cmd.none)

        Edit _ (Err _) ->
            (model, Cmd.none)

        Edit oldItem (Ok newItem) ->
            ({ model | state = Listing, items = Aptly.Generic.replace model.items oldItem newItem }, Cmd.none)

        Request state ->
            case model.state of
                Listing ->
                    (model, Http.send List <| Tuple.first factory.list)

                Creating newItem ->
                    case factory.create of
                        Nothing ->
                            (model, Cmd.none)

                        Just (createRequest, _) ->
                            (model, Http.send Create <| createRequest newItem)

                Deleting oldItem ->
                    case factory.delete of
                        Nothing ->
                            (model, Cmd.none)

                        Just (deleteRequest, _) ->
                            (model, Http.send (Delete oldItem) <| deleteRequest oldItem)

                Editing oldItem newItem ->
                    case factory.edit of
                        Nothing ->
                            (model, Cmd.none)

                        Just (editRequest, _) ->
                            (model, Http.send (Edit oldItem) <| editRequest oldItem newItem)

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

view : RequestFactory a b -> a ->  Model a -> Html.Html (Msg a b)
view requestFactory newItem model =
    Html.div []
        <| case model.state of
            Listing ->
                let
                    (listRequest, listView) =
                        requestFactory.list
                in
                    List.append
                        (List.concat
                            [ [ Html.h1 [] [ Html.text "Local Repositories" ] ]
                            , if requestFactory.create /= Nothing then [ Html.button [ Html.Events.onClick <| State <| Creating newItem ] [ Html.text "Create" ] ] else []
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
