module PublishedRepositoryPage exposing (..)

import Aptly.Generic
import Aptly.Published.Repository
import Html
import Http

type alias Model =
    { repositories : List Aptly.Published.Repository.Repository
    , server : String
    , state : State
    }

type alias ChangeSet =
    { old : Aptly.Published.Repository.Repository
    , new : Aptly.Published.Repository.Repository
    }

type State
    = Listing
    | Changing ChangeSet

type Msg
    = List (Result Http.Error (List Aptly.Published.Repository.Repository))
    | State State
    | Update Aptly.Published.Repository.Repository (Result Http.Error (Aptly.Published.Repository.Repository))
    | Request ((Result Http.Error (Aptly.Published.Repository.Repository)) -> Msg) (Http.Request Aptly.Published.Repository.Repository)

init : String -> (Model, Cmd Msg)
init server =
    (Model [] server Listing, Aptly.Published.Repository.createListRequest server |> Http.send List)

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

        Request result request ->
            (model, Http.send result request)

        Update oldRepository (Err _) ->
            (model, Cmd.none)

        Update oldRepository (Ok newRepository) ->
            ({ model | repositories = Aptly.Generic.replace model.repositories oldRepository newRepository }, Cmd.none)

updateMsg : String -> Aptly.Published.Repository.Repository -> Aptly.Published.Repository.Repository -> Msg
updateMsg server oldRepository newRepository =
    Request (Update oldRepository) <| Aptly.Published.Repository.createEditRequest server newRepository

view model =
    Html.div []
        <| List.append
            [ Html.h1 [] [ Html.text "Published Repositories" ]
            ]
            <| case model.state of
                Listing ->
                    List.intersperse (Html.hr [] []) <| List.map (Aptly.Published.Repository.view (\repository -> State <| Changing <| ChangeSet repository repository)) model.repositories

                Changing changeSet ->
                    [ Aptly.Published.Repository.viewForm (State Listing) (updateMsg model.server changeSet.old) changeSet.new ]
