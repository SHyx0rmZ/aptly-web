module LocalRepositoryPage exposing (..)

import Aptly.Config
import Aptly.Generic
import Aptly.Local.Repository
import Debug
import Html
import Html.Attributes
import Html.Events
import Http

type alias Model =
    { config : Aptly.Config.Config
    , repositories : List Aptly.Local.Repository.Repository
    , state : State
    , force : Bool
    }

type Msg
    = List (Result Http.Error (List Aptly.Local.Repository.Repository))
    | State State
    | FinishChangingResult (Maybe Aptly.Local.Repository.Repository) (Result Http.Error Aptly.Local.Repository.Repository)
    | FinishChanging (Maybe Aptly.Local.Repository.Repository) Aptly.Local.Repository.Repository
    | FinishDeletingResult Aptly.Local.Repository.Repository (Result Http.Error String)
    | FinishDeleting Aptly.Local.Repository.Repository
    | RepositoryMsg Aptly.Local.Repository.Msg
    | SetForce Bool

type alias ChangeSet =
    { old : Maybe Aptly.Local.Repository.Repository
    , new : Maybe Aptly.Local.Repository.Repository
    }

type State
    = Listing
    | Changing ChangeSet

init : Aptly.Config.Config -> (Model, Cmd Msg)
init config =
    (Model config [] Listing False, Aptly.Local.Repository.createListRequest config.server |> Http.send List)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        List (Err _) ->
            (model, Cmd.none)

        List (Ok repositories) ->
            ({ model | repositories = List.sortBy .name repositories }, Cmd.none)

        State Listing ->
            ({ model | state = Listing, force = False }, Cmd.none)

        State (Changing changeSet) ->
            ({ model | state = Changing changeSet }, Cmd.none)

        FinishChangingResult _ (Err _) ->
            (model, Cmd.none)

        FinishChangingResult (Just oldRepository) (Ok newRepository) ->
            ({ model | repositories = Aptly.Generic.replace model.repositories oldRepository newRepository, state = Listing }, Cmd.none)

        FinishChangingResult (Nothing) (Ok newRepository) ->
            ({ model | repositories = List.sortBy .name <| newRepository :: model.repositories, state = Listing }, Cmd.none)

        FinishChanging (Just oldRepository) newRepository ->
            (model, Http.send (FinishChangingResult <| Just oldRepository) (Aptly.Local.Repository.createEditRequest model.config.server newRepository))

        FinishChanging Nothing newRepository ->
            (model, Http.send (FinishChangingResult <| Nothing) (Aptly.Local.Repository.createCreateRequest model.config.server newRepository))

        FinishDeletingResult _ (Err _) ->
            (model, Cmd.none)

        FinishDeletingResult repositoryToDelete (Ok _) ->
            ({ model | state = Listing, force = False, repositories = List.filter (\repository -> repository /= repositoryToDelete) model.repositories }, Cmd.none)

        FinishDeleting repository ->
            (model, Http.send (FinishDeletingResult <| repository) (Aptly.Local.Repository.createDeleteRequest model.config.server repository model.force))

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

        SetForce force ->
            ({ model | force = force }, Cmd.none)

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
                            [ Aptly.Local.Repository.viewForm False RepositoryMsg (State Listing) (FinishChanging changeSet.old) newRepository ]

                        (Nothing, Just newRepository) ->
                            [ Aptly.Local.Repository.viewForm True RepositoryMsg (State Listing) (FinishChanging changeSet.old) newRepository ]

                        (Just oldRepository, Nothing) ->
                            [ Html.p [] [ Html.text <| "Are you sure you want to delete the repository\"" ++ oldRepository.name ++ "\"?" ]
                            , Html.strong [] [ Html.text "Warning!" ]
                            , Html.text " This action cannot be undone!"
                            , Html.div []
                                [ Html.input [ Html.Events.onClick <| SetForce <| not model.force, Html.Attributes.type_ "checkbox", Html.Attributes.checked model.force ] []
                                , Html.text "Force"
                                ]
                            , Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]
                            , Html.button [ Html.Events.onClick <| FinishDeleting oldRepository ] [ Html.text "Delete" ]
                            ]
