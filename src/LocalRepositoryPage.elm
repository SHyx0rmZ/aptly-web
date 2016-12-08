module LocalRepositoryPage exposing (..)

import Aptly.Local.Repository
import Debug
import Html
import Html.Events
import Http

type alias Model =
    { repositories : List Aptly.Local.Repository.Repository
    , state : State
    }

type Msg
    = List (Result Http.Error (List Aptly.Local.Repository.Repository))
    | State State
    | CancelEditing
    | FinishEditingResult (Maybe Aptly.Local.Repository.Repository) (Result Http.Error Aptly.Local.Repository.Repository)
    | FinishEditing (Maybe Aptly.Local.Repository.Repository) Aptly.Local.Repository.Repository
    | RepositoryMsg Aptly.Local.Repository.Msg
    | Delete Aptly.Local.Repository.Repository


type alias ChangeSet =
    { old : Maybe Aptly.Local.Repository.Repository
    , new : Maybe Aptly.Local.Repository.Repository
    }

type State
    = Listing
    | Changing ChangeSet

init : (Model, Cmd Msg)
init =
    (Model [] Listing Nothing, Cmd.none)

replace : List a -> a -> a -> List a
replace list old new =
    List.map (\item ->
        if item == old then
            new
        else
            item) list

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        List (Err _) ->
            (model, Cmd.none)

        List (Ok repositories) ->
            ({ model | repositories = repositories }, Cmd.none)

        State Listing ->
            ({ model | state = Listing }, Cmd.none)

        State (Changing changeSet) ->
            ({ model | state = Changing changeSet }, Cmd.none)

        CancelEditing ->
            ({ model | state = Listing }, Cmd.none)

        FinishEditingResult _ (Err _) ->
            (model, Cmd.none)

        FinishEditingResult (Just oldRepository) (Ok newRepository) ->
            ({ model | repositories = replace model.repositories oldRepository newRepository, state = Listing }, Cmd.none)

        FinishEditingResult (Nothing) (Ok newRepository) ->
            ({ model | repositories = newRepository :: model.repositories, state = Listing }, Cmd.none)

        FinishEditing (Just oldRepository) newRepository ->
            (model, Http.send (FinishEditingResult <| Just oldRepository) (Aptly.Local.Repository.createEditRequest "http://127.0.0.1:8080" newRepository))

        FinishEditing Nothing newRepository ->
            (model, Http.send (FinishEditingResult <| Nothing) (Aptly.Local.Repository.createCreateRequest "http://127.0.0.1:8080" newRepository))

        RepositoryMsg msg ->
            case model.state of
                Changing changeSet ->
                    let
                        (repositoryModel, repositoryMsg) =
                            Aptly.Local.Repository.update msg changeSet.new
                    in
                        ({ model | state = Changing { changeSet | new = repositoryModel } }, Cmd.map RepositoryMsg repositoryMsg)

                Listing ->
                    (model, Cmd.none)

view : Model -> Html.Html Msg
view model =
    Html.div []
        <| List.append
            [ Html.h1 [] [ Html.text "Local Repositories" ]
            , Html.button [ Html.Events.onClick (State <| Changing <| ChangeSet Nothing <| Just <| Aptly.Local.Repository.Repository "" "" "" "") ] [ Html.text "Create" ]
            , Html.hr [] []
            ]
            <| case model.state of
                Listing ->
                    (List.intersperse (Html.hr [] []) <| List.map (Aptly.Local.Repository.view (\repository -> State <| Changing <| ChangeSet (Just repository) (Just repository)) Delete) model.repositories)

                Changing changeSet ->
                    case (changeSet.old, changeSet.new) of
                        (Nothing, Nothing) ->
                            []

                        (Just oldRepository, Just newRepository) ->
                            [ Aptly.Local.Repository.viewForm False RepositoryMsg CancelEditing (FinishEditing changeSet.old) newRepository ]

                        (Nothing, Just newRepository) ->
                            [ Aptly.Local.Repository.viewForm True RepositoryMsg CancelEditing (FinishEditing changeSet.old) newRepository ]

                        (Just oldRepository, Nothing) ->
                            []
