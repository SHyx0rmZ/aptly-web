module Main exposing (..)

import Aptly.Config
import FilePage
import Html
import Html.Events
import LocalRepositoryPage
import PackagePage
import PublishedRepositoryPage
import SnapshotPage

{-

    TODO

    - [ ] make generic list sorted
    - [/] upload files to local repository
    - [x] create snapshot from local repository
    - [ ] create published repository from snapshots
    - [ ] delete multiple snapshots at once

-}

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

type Page
    = File
    | LocalRepository
    | Package
    | PublishedRepository
    | Snapshot


type alias Model =
    { page : Page
    , config : Aptly.Config.Config
    , file : FilePage.Model
    , localRepository : LocalRepositoryPage.Model
    , package : PackagePage.Model
    , publishedRepository : PublishedRepositoryPage.Model
    , snapshot : SnapshotPage.Model
    }

type Msg
    = FileMsg FilePage.Msg
    | LocalRepositoryMsg LocalRepositoryPage.Msg
    | PackageMsg PackagePage.Msg
    | Page Page
    | PublishedRepositoryMsg PublishedRepositoryPage.Msg
    | SnapshotMsg SnapshotPage.Msg

init : (Model, Cmd Msg)
init =
    let
        config = Aptly.Config.Config "http://127.0.0.1:8080"

        (filePageModel, filePageMsg) =
            FilePage.init config

        (localRepositoryPageModel, localRepositoryPageMsg) =
            LocalRepositoryPage.init config

        (packagePageModel, packagePageMsg) =
            PackagePage.init config

        (publishedRepositoryPageModel, publishedRepositoryPageMsg) =
            PublishedRepositoryPage.init config

        (snapshotPageModel, snapshotPageMsg) =
            SnapshotPage.init config
    in
        ( Model LocalRepository config filePageModel localRepositoryPageModel packagePageModel publishedRepositoryPageModel snapshotPageModel
        , Cmd.batch
            [ Cmd.map FileMsg filePageMsg
            , Cmd.map LocalRepositoryMsg localRepositoryPageMsg
            , Cmd.map PackageMsg packagePageMsg
            , Cmd.map PublishedRepositoryMsg publishedRepositoryPageMsg
            , Cmd.map SnapshotMsg snapshotPageMsg
            ]
        )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FileMsg fileMsg ->
            let
                (filePageModel, filePageMsg) =
                    FilePage.update fileMsg model.file
            in
                ({ model | file = filePageModel }, Cmd.map FileMsg filePageMsg)

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
        [ Html.button [ Html.Events.onClick <| Page File ] [ Html.text "Files" ]
        , Html.button [ Html.Events.onClick <| Page LocalRepository ] [ Html.text "Local Repositories" ]
        , Html.button [ Html.Events.onClick <| Page Package ] [ Html.text "Packages" ]
        , Html.button [ Html.Events.onClick <| Page PublishedRepository ] [ Html.text "Published Repositories" ]
        , Html.button [ Html.Events.onClick <| Page Snapshot ] [ Html.text "Snapshots" ]
        , Html.hr [] []
        , case model.page of
            File ->
                Html.map FileMsg <| FilePage.view model.file

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
        [ Sub.map FileMsg <| FilePage.subscriptions model.file
        , Sub.map LocalRepositoryMsg <| LocalRepositoryPage.subscriptions model.localRepository
        , Sub.map PublishedRepositoryMsg <| PublishedRepositoryPage.subscriptions model.publishedRepository
        , Sub.map SnapshotMsg <| SnapshotPage.subscriptions model.snapshot
        ]
