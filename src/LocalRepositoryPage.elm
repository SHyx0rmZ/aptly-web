module LocalRepositoryPage exposing (..)

import Aptly.Local.Repository
import Debug
import Html
import Html.Events
import Http

type alias Model =
    { repositories : List Aptly.Local.Repository.Repository
    , state : State
    , server : String
    }

type Msg
    = List (Result Http.Error (List Aptly.Local.Repository.Repository))
    | State State
    | CancelChanging
    | FinishChangingResult (Maybe Aptly.Local.Repository.Repository) (Result Http.Error Aptly.Local.Repository.Repository)
    | FinishChanging (Maybe Aptly.Local.Repository.Repository) Aptly.Local.Repository.Repository
    | FinishDeletingResult Aptly.Local.Repository.Repository (Result Http.Error String)
    | FinishDeleting Aptly.Local.Repository.Repository
    | RepositoryMsg Aptly.Local.Repository.Msg

type alias ChangeSet =
    { old : Maybe Aptly.Local.Repository.Repository
    , new : Maybe Aptly.Local.Repository.Repository
    }

type State
    = Listing
    | Changing ChangeSet

init : String -> (Model, Cmd Msg)
init server =
    (Model [] Listing server, Cmd.none)

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

        CancelChanging ->
            ({ model | state = Listing }, Cmd.none)

        FinishChangingResult _ (Err _) ->
            (model, Cmd.none)

        FinishChangingResult (Just oldRepository) (Ok newRepository) ->
            ({ model | repositories = replace model.repositories oldRepository newRepository, state = Listing }, Cmd.none)

        FinishChangingResult (Nothing) (Ok newRepository) ->
            ({ model | repositories = newRepository :: model.repositories, state = Listing }, Cmd.none)

        FinishChanging (Just oldRepository) newRepository ->
            (model, Http.send (FinishChangingResult <| Just oldRepository) (Aptly.Local.Repository.createEditRequest model.server newRepository))

        FinishChanging Nothing newRepository ->
            (model, Http.send (FinishChangingResult <| Nothing) (Aptly.Local.Repository.createCreateRequest model.server newRepository))

        FinishDeletingResult _ (Err _) ->
            (model, Cmd.none)

        FinishDeletingResult repositoryToDelete (Ok _) ->
            ({ model | state = Listing, repositories = List.filter (\repository -> repository /= repositoryToDelete) model.repositories }, Cmd.none)

        FinishDeleting repository ->
            (model, Http.send (FinishDeletingResult <| repository) (Aptly.Local.Repository.createDeleteRequest model.server repository False))

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
                    (List.intersperse (Html.hr [] []) <| List.map
                        (Aptly.Local.Repository.view
                            (\repository -> State <| Changing <| ChangeSet (Just repository) (Just repository))
                            (\repository -> State <| Changing <| ChangeSet (Just repository) Nothing)
                        )
                        model.repositories)

                Changing changeSet ->
                    case (changeSet.old, changeSet.new) of
                        (Nothing, Nothing) ->
                            []

                        (Just oldRepository, Just newRepository) ->
                            [ Aptly.Local.Repository.viewForm False RepositoryMsg CancelChanging (FinishChanging changeSet.old) newRepository ]

                        (Nothing, Just newRepository) ->
                            [ Aptly.Local.Repository.viewForm True RepositoryMsg CancelChanging (FinishChanging changeSet.old) newRepository ]

                        (Just oldRepository, Nothing) ->
                            [ Html.p [] [ Html.text <| "Are you sure you want to delete the repository\"" ++ oldRepository.name ++ "\"?" ]
                            , Html.strong [] [ Html.text "Warning!" ]
                            , Html.text " This action cannot be undone!"
                            , Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]
                            , Html.button [ Html.Events.onClick <| FinishDeleting oldRepository ] [ Html.text "Delete" ]
                            ]
