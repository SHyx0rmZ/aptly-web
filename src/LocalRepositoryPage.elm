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
    | Change (Maybe Aptly.Local.Repository.Repository) (Result Http.Error Aptly.Local.Repository.Repository)
    | Delete Aptly.Local.Repository.Repository (Result Http.Error String)
    | Request ((Result Http.Error String) -> Msg) (Http.Request String)
    | RequestWithBody ((Result Http.Error Aptly.Local.Repository.Repository) -> Msg) (Http.Request Aptly.Local.Repository.Repository)
    | RepositoryMsg Aptly.Local.Repository.Msg
    | Force Bool

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

createMsg : String -> Maybe Aptly.Local.Repository.Repository -> Aptly.Local.Repository.Repository -> Msg
createMsg server oldRepository newRepository =
    RequestWithBody (Change oldRepository) <| Aptly.Local.Repository.createCreateRequest server newRepository

deleteMsg : String -> Bool -> Aptly.Local.Repository.Repository -> Msg
deleteMsg server force oldRepository =
    Request (Delete oldRepository) <| Aptly.Local.Repository.createDeleteRequest server force oldRepository

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

        Change _ (Err _) ->
            (model, Cmd.none)

        Change (Just oldRepository) (Ok newRepository) ->
            ({ model | repositories = Aptly.Generic.replace model.repositories oldRepository newRepository, state = Listing }, Cmd.none)

        Change (Nothing) (Ok newRepository) ->
            ({ model | repositories = List.sortBy .name <| newRepository :: model.repositories, state = Listing }, Cmd.none)

        Delete _ (Err _) ->
            (model, Cmd.none)

        Delete repositoryToDelete (Ok _) ->
            ({ model | state = Listing, force = False, repositories = List.filter (\repository -> repository /= repositoryToDelete) model.repositories }, Cmd.none)

        Request result request ->
            (model, Http.send result request)

        RequestWithBody result request ->
            (model, Http.send result request)

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

        Force force ->
            ({ model | force = force }, Cmd.none)

updateMsg : String -> Maybe Aptly.Local.Repository.Repository -> Aptly.Local.Repository.Repository -> Msg
updateMsg server oldRepository newRepository =
    RequestWithBody (Change oldRepository) <| Aptly.Local.Repository.createEditRequest server newRepository

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
                            [ Aptly.Local.Repository.viewForm False RepositoryMsg (State Listing) (updateMsg model.config.server changeSet.old) newRepository ]

                        (Nothing, Just newRepository) ->
                            [ Aptly.Local.Repository.viewForm True RepositoryMsg (State Listing) (createMsg model.config.server changeSet.old) newRepository ]

                        (Just oldRepository, Nothing) ->
                            [ Html.p [] [ Html.text <| "Are you sure you want to delete the repository\"" ++ oldRepository.name ++ "\"?" ]
                            , Html.strong [] [ Html.text "Warning!" ]
                            , Html.text " This action cannot be undone!"
                            , Html.div []
                                [ Html.input [ Html.Events.onClick <| Force <| not model.force, Html.Attributes.type_ "checkbox", Html.Attributes.checked model.force ] []
                                , Html.text "Force"
                                ]
                            , Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]
                            , Html.button [ Html.Events.onClick <| deleteMsg model.config.server model.force oldRepository ] [ Html.text "Delete" ]
                            ]
