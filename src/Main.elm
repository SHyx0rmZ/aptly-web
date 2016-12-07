module Main exposing (..)

import Aptly.Local.Repository
import Aptly.Published.Repository
import Aptly.Source
import Html
import Http
import Json.Decode
import LocalRepositoryPage
import PublishedRepositoryPage

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

type Page
    = LocalRepository
    | PublishedRepository

type alias Model =
    { page : Page
    , localRepository : LocalRepositoryPage.Model
    , publishedRepository : PublishedRepositoryPage.Model
    }

type Msg
    = LocalRepositoryMsg LocalRepositoryPage.Msg
    | PublishedRepositoryMsg PublishedRepositoryPage.Msg

init : (Model, Cmd Msg)
init =
    let
        (localRepositoryPageModel, localRepositoryPageMsg) =
            LocalRepositoryPage.init

        (publishedRepositoryPageModel, publishedRepositoryPageMsg) =
            PublishedRepositoryPage.init
    in
        ( Model LocalRepository localRepositoryPageModel publishedRepositoryPageModel
        , Cmd.batch
--            [ Cmd.map LocalRepositoryMsg <| Http.send LocalRepositoryPage.List <| Aptly.Local.Repository.list "http://127.0.0.1:8080"
            [ Aptly.Local.Repository.list "http://127.0.0.1:8080"
                |> Http.send LocalRepositoryPage.List
                |> Cmd.map LocalRepositoryMsg
            , Cmd.map LocalRepositoryMsg localRepositoryPageMsg
            , Cmd.map PublishedRepositoryMsg publishedRepositoryPageMsg
            ]
        )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LocalRepositoryMsg localRepositoryMsg ->
            let
                (localRepositoryPageModel, localRepositoryPageMsg) =
                    LocalRepositoryPage.update localRepositoryMsg model.localRepository
            in
                ({ model | localRepository = localRepositoryPageModel }, Cmd.map LocalRepositoryMsg localRepositoryPageMsg)

        PublishedRepositoryMsg publishedRepositoryMsg ->
            let
                (publishedRepositoryPageModel, publishedRepositoryPageMsg) =
                    PublishedRepositoryPage.update publishedRepositoryMsg model.publishedRepository
            in
                ({ model | publishedRepository = publishedRepositoryPageModel }, Cmd.map PublishedRepositoryMsg publishedRepositoryPageMsg)

view : Model -> Html.Html Msg
view model =
    case model.page of
        LocalRepository ->
            Html.map LocalRepositoryMsg <| LocalRepositoryPage.view model.localRepository

        PublishedRepository ->
            Html.map PublishedRepositoryMsg <| PublishedRepositoryPage.view model.publishedRepository

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
