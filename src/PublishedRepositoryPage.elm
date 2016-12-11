module PublishedRepositoryPage exposing (..)

import Aptly.Config
import Aptly.Generic
import Aptly.Published.Repository
import Html
import Html.Attributes
import Html.Events
import Http

type alias Model =
    { config : Aptly.Config.Config
    , repositories : List Aptly.Published.Repository.Repository
    , state : State
    , force : Bool
    }

type alias ChangeSet =
    { old : Aptly.Published.Repository.Repository
    , new : Maybe Aptly.Published.Repository.Repository
    }

type State
    = Listing
    | Changing ChangeSet

type Msg
    = List (Result Http.Error (List Aptly.Published.Repository.Repository))
    | State State
    | Update Aptly.Published.Repository.Repository (Result Http.Error (Aptly.Published.Repository.Repository))
    | Delete Aptly.Published.Repository.Repository (Result Http.Error String)
    | Request ((Result Http.Error String) -> Msg) (Http.Request String)
    | RequestWithBody ((Result Http.Error (Aptly.Published.Repository.Repository)) -> Msg) (Http.Request Aptly.Published.Repository.Repository)
    | Force Bool

deleteMsg : String -> Bool -> Aptly.Published.Repository.Repository -> Msg
deleteMsg server force oldRepository =
    Request (Delete oldRepository) <| Aptly.Published.Repository.createDeleteRequest server force oldRepository

init : Aptly.Config.Config -> (Model, Cmd Msg)
init config  =
    (Model config [] Listing False, Aptly.Published.Repository.createListRequest config.server |> Http.send List)

update msg model =
    case msg of
        List (Err _) ->
            (model, Cmd.none)

        List (Ok repositories) ->
            ({ model | repositories = repositories }, Cmd.none)

        State Listing ->
            ({ model | state = Listing, force = False }, Cmd.none)

        State (Changing changeSet) ->
            ({ model | state = Changing changeSet }, Cmd.none)

        Request result request ->
            (model, Http.send result request)

        RequestWithBody result request ->
            (model, Http.send result request)

        Update _ (Err _) ->
            (model, Cmd.none)

        Update oldRepository (Ok newRepository) ->
            ({ model | state = Listing, repositories = Aptly.Generic.replace model.repositories oldRepository newRepository }, Cmd.none)

        Delete _ (Err _) ->
            (model, Cmd.none)

        Delete oldRepository (Ok _) ->
            ({ model | state = Listing, force = False, repositories = List.filter (\repository -> repository /= oldRepository) model.repositories }, Cmd.none)

        Force force ->
            ({ model | force = force }, Cmd.none)

updateMsg : String -> Aptly.Published.Repository.Repository -> Aptly.Published.Repository.Repository -> Msg
updateMsg server oldRepository newRepository =
    RequestWithBody (Update oldRepository) <| Aptly.Published.Repository.createEditRequest server newRepository

view model =
    Html.div []
        <| List.append
            [ Html.h1 [] [ Html.text "Published Repositories" ]
            ]
            <| case model.state of
                Listing ->
                    List.intersperse (Html.hr [] [])
                        <| List.map (Aptly.Published.Repository.view
                            (\repository -> State <| Changing <| ChangeSet repository (Just repository))
                            (\repository -> State <| Changing <| ChangeSet repository Nothing)
                        ) model.repositories

                Changing changeSet ->
                    case (changeSet.old, changeSet.new) of
                        (_, Just newRepository) ->
                            [ Aptly.Published.Repository.viewForm (State Listing) (updateMsg model.config.server changeSet.old) newRepository ]

                        (_, Nothing) ->
                            [ Html.p [] [ Html.text <| "Are you sure you want to unpublish the repository \"" ++ changeSet.old.prefix ++ "/" ++ changeSet.old.distribution ++ "\"?" ]
                            , Html.strong [] [ Html.text "Warning!" ]
                            , Html.text " This action cannot be undone!"
                            , Html.div []
                                [ Html.input [ Html.Events.onClick <| Force <| not model.force, Html.Attributes.type_ "checkbox", Html.Attributes.checked model.force ] []
                                , Html.text "Force"
                                ]
                            , Html.button [ Html.Events.onClick <| State Listing ] [ Html.text "Cancel" ]
                            , Html.button [ Html.Events.onClick <| (deleteMsg model.config.server model.force changeSet.old) ] [ Html.text "Delete" ]
                            ]
