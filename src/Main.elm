module Main exposing (..)

import Aptly.Local.Repository
import Aptly.Published.Repository
import Aptly.Source
import Html
import Html.Events
import Http
import Json.Decode
import LocalRepositoryPage
import PackagePage
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
    | Package
    | PublishedRepository

type alias Model =
    { page : Page
    , server : String
    , localRepository : LocalRepositoryPage.Model
    , package : PackagePage.Model
    , publishedRepository : PublishedRepositoryPage.Model
    }

type Msg
    = LocalRepositoryMsg LocalRepositoryPage.Msg
    | PackageMsg PackagePage.Msg
    | Page Page
    | PublishedRepositoryMsg PublishedRepositoryPage.Msg

init : (Model, Cmd Msg)
init =
    let
        server = "http://127.0.0.1:8080"

        (localRepositoryPageModel, localRepositoryPageMsg) =
            LocalRepositoryPage.init server

        (packagePageModel, packagePageMsg) =
            PackagePage.init server

        (publishedRepositoryPageModel, publishedRepositoryPageMsg) =
            PublishedRepositoryPage.init server
    in
        ( Model LocalRepository server localRepositoryPageModel packagePageModel publishedRepositoryPageModel
        , Cmd.batch
            [ Cmd.map LocalRepositoryMsg localRepositoryPageMsg
            , Cmd.map PackageMsg packagePageMsg
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

        PackageMsg packageMsg ->
            let
                (packagePageModel, packagePageMsg) =
                    PackagePage.update packageMsg model.package
            in
                ({ model | package = packagePageModel }, Cmd.map PackageMsg packagePageMsg)

        Page page ->
            ({ model | page = page }, Cmd.none)

        PublishedRepositoryMsg publishedRepositoryMsg ->
            let
                (publishedRepositoryPageModel, publishedRepositoryPageMsg) =
                    PublishedRepositoryPage.update publishedRepositoryMsg model.publishedRepository
            in
                ({ model | publishedRepository = publishedRepositoryPageModel }, Cmd.map PublishedRepositoryMsg publishedRepositoryPageMsg)

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button [ Html.Events.onClick <| Page LocalRepository ] [ Html.text "Local Repositories" ]
        , Html.button [ Html.Events.onClick <| Page Package ] [ Html.text "Packages" ]
        , Html.button [ Html.Events.onClick <| Page PublishedRepository ] [ Html.text "Published Repositories" ]
        , Html.hr [] []
        , case model.page of
            LocalRepository ->
                Html.map LocalRepositoryMsg <| LocalRepositoryPage.view model.localRepository

            Package ->
                Html.map PackageMsg <| PackagePage.view model.package

            PublishedRepository ->
                Html.map PublishedRepositoryMsg <| PublishedRepositoryPage.view model.publishedRepository
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
