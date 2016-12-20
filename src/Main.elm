module Main exposing (..)

import Aptly.Config
import Html
import Html.Events
import LocalRepositoryPage
import PackagePage
import PublishedRepositoryPage
import SnapshotPage

main : Program Never Model Msg
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
    | Snapshot

type alias Model =
    { page : Page
    , config : Aptly.Config.Config
    , localRepository : LocalRepositoryPage.Model
    , package : PackagePage.Model
    , publishedRepository : PublishedRepositoryPage.Model
    , snapshot : SnapshotPage.Model
    }

type Msg
    = LocalRepositoryMsg LocalRepositoryPage.Msg
    | PackageMsg PackagePage.Msg
    | Page Page
    | PublishedRepositoryMsg PublishedRepositoryPage.Msg
    | SnapshotMsg SnapshotPage.Msg

init : (Model, Cmd Msg)
init =
    let
        config = Aptly.Config.Config "http://127.0.0.1:8080"

        (localRepositoryPageModel, localRepositoryPageMsg) =
            LocalRepositoryPage.init config

        (packagePageModel, packagePageMsg) =
            PackagePage.init config

        (publishedRepositoryPageModel, publishedRepositoryPageMsg) =
            PublishedRepositoryPage.init config

        (snapshotPageModel, snapshotPageMsg) =
            SnapshotPage.init config
    in
        ( Model LocalRepository config localRepositoryPageModel packagePageModel publishedRepositoryPageModel snapshotPageModel
        , Cmd.batch
            [ Cmd.map LocalRepositoryMsg localRepositoryPageMsg
            , Cmd.map PackageMsg packagePageMsg
            , Cmd.map PublishedRepositoryMsg publishedRepositoryPageMsg
            , Cmd.map SnapshotMsg snapshotPageMsg
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

        SnapshotMsg snapshotMsg ->
            let
                (snapshotPageModel, snapshotPageMsg) =
                    SnapshotPage.update snapshotMsg model.snapshot
            in
                ({ model | snapshot = snapshotPageModel }, Cmd.map SnapshotMsg snapshotPageMsg)

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button [ Html.Events.onClick <| Page LocalRepository ] [ Html.text "Local Repositories" ]
        , Html.button [ Html.Events.onClick <| Page Package ] [ Html.text "Packages" ]
        , Html.button [ Html.Events.onClick <| Page PublishedRepository ] [ Html.text "Published Repositories" ]
        , Html.button [ Html.Events.onClick <| Page Snapshot ] [ Html.text "Snapshots" ]
        , Html.hr [] []
        , case model.page of
            LocalRepository ->
                Html.map LocalRepositoryMsg <| LocalRepositoryPage.view model.localRepository

            Package ->
                Html.map PackageMsg <| PackagePage.view model.package

            PublishedRepository ->
                Html.map PublishedRepositoryMsg <| PublishedRepositoryPage.view model.publishedRepository

            Snapshot ->
                Html.map SnapshotMsg <| SnapshotPage.view model.snapshot
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map PublishedRepositoryMsg <| PublishedRepositoryPage.subscriptions model.publishedRepository
        , Sub.map SnapshotMsg <| SnapshotPage.subscriptions model.snapshot
        ]
