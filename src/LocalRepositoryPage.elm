module LocalRepositoryPage exposing (..)

import Aptly.Local.Repository
import Debug
import Html
import Http

type alias Model =
    { repositories : List Aptly.Local.Repository.Repository
    , state : State
    }

type Msg
    = List (Result Http.Error (List Aptly.Local.Repository.Repository))
    | State State
    | CancelEditing
    | FinishEditingResult Aptly.Local.Repository.Repository (Result Http.Error Aptly.Local.Repository.Repository)
    | FinishEditing Aptly.Local.Repository.Repository Aptly.Local.Repository.Repository
    | RepositoryMsg ChangeSet Aptly.Local.Repository.Msg


type alias ChangeSet =
    { old : Aptly.Local.Repository.Repository
    , new : Aptly.Local.Repository.Repository
    }

type State
    = Listing
    | Editing ChangeSet

init : (Model, Cmd Msg)
init =
    (Model [] Listing, Cmd.none)

replace : List a -> a -> a -> List a
replace list old new =
    List.map (\item ->
        case item == old of
            True -> new
            False -> item) list

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        List (Err _) ->
            (model, Cmd.none)

        List (Ok repositories) ->
            ({ model | repositories = repositories }, Cmd.none)

        State Listing ->
            ({ model | state = Listing }, Cmd.none)

        State (Editing repository) ->
            ({ model | state = Editing repository }, Cmd.none)

        CancelEditing ->
            ({ model | state = Listing }, Cmd.none)

        FinishEditingResult _ (Err _) ->
            (model, Cmd.none)

        FinishEditingResult oldRepository (Ok newRepository) ->
            let
                _ = Debug.log "old" oldRepository
                _ = Debug.log "new" newRepository
            in
                ({ model | repositories = replace model.repositories oldRepository newRepository, state = Listing }, Cmd.none)

        FinishEditing oldRepository newRepository ->
            (model, Http.send (FinishEditingResult oldRepository) (Aptly.Local.Repository.edit "http://127.0.0.1:8080" newRepository))

        RepositoryMsg changeSet msg ->
            let
                (repositoryModel, repositoryMsg) =
                    Aptly.Local.Repository.update msg changeSet.new
            in
                ({ model | state = Editing { changeSet | new = repositoryModel } }, Cmd.map (RepositoryMsg { changeSet | new = repositoryModel }) repositoryMsg)

view : Model -> Html.Html Msg
view model =
    Html.div []
        <| List.append
            [ Html.h1 [] [ Html.text "Local Repositories" ]
            ]
            <| case model.state of
                Listing ->
                    (List.intersperse (Html.hr [] []) <| List.map (Aptly.Local.Repository.view (\repository -> State (Editing <| ChangeSet repository repository))) model.repositories)

                Editing changeSet ->
                    [ Aptly.Local.Repository.viewForm False (RepositoryMsg changeSet) (CancelEditing) (FinishEditing changeSet.old) changeSet.new ]
