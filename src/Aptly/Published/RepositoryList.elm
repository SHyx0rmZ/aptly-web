module Aptly.Published.RepositoryList exposing (..)

import Aptly.Config
import Aptly.Generic.List
import Aptly.Published.Repository
import Aptly.SigningOptions
import Aptly.SnapshotList
import Aptly.Source
import Html
import Task

type alias RepositoryList =
    { config : Aptly.Config.Config
    , list : Aptly.Generic.List.Model Aptly.Published.Repository.Repository
    , force : Bool
    , newArchitecture : String
    , newSource : Aptly.Source.Source
    }

type Msg
    = ListMsg (Aptly.Generic.List.Msg Aptly.Published.Repository.Repository Aptly.Published.Repository.Msg Msg)
    | Force Bool
    | NewArchitectureChanged String
    | NewSourceChanged Aptly.Source.Source

factory : Aptly.SigningOptions.SigningOptions -> String -> Aptly.Source.Source -> Bool -> String -> Aptly.Generic.List.RequestFactory Aptly.Published.Repository.Repository Aptly.Published.Repository.Msg Msg
factory signing newArchitecture newSource force server =
    { create = Just (Aptly.Published.Repository.createCreateRequest signing server, Aptly.Published.Repository.viewCreate newArchitecture newSource (Aptly.Generic.List.mapMsg << NewArchitectureChanged) (Aptly.Generic.List.mapMsg << NewSourceChanged))
    , delete = Just (Aptly.Published.Repository.createDeleteRequest force server, Aptly.Published.Repository.viewConfirmation force (\force -> Aptly.Generic.List.mapMsg (Force force)))
    , edit = Just (Aptly.Published.Repository.createEditRequest signing server, Aptly.Published.Repository.viewForm)
    , list = (Aptly.Published.Repository.createListRequest server, Aptly.Published.Repository.view)
    , listExtra = Nothing
    }

init : Aptly.Config.Config -> (RepositoryList, Cmd Msg)
init config =
    let
        (listModel, listMsg) =
            Aptly.Generic.List.init (factory config.signing "" (Aptly.Source.Source "" "") False config.server)
    in
        (RepositoryList config listModel False "" <| Aptly.Source.Source "" "", Cmd.map ListMsg listMsg)

update : Msg -> RepositoryList -> (RepositoryList, Cmd Msg)
update msg repositoryList =
    case msg of
        ListMsg (Aptly.Generic.List.ParentMsg msg) ->
            (repositoryList, Task.perform (\() -> msg) <| Task.succeed ())

        ListMsg (Aptly.Generic.List.State (Aptly.Generic.List.Creating newRepository)) ->
            let
                (snapshotListModel, snapshotListMsg) =
                    Aptly.SnapshotList.init repositoryList.config

                newMsg =
                    Aptly.Generic.List.State (Aptly.Generic.List.Creating { newRepository | snapshotList = Just snapshotListModel })

                (listModel, listMsg) =
                    Aptly.Generic.List.update Aptly.Published.Repository.update (factory repositoryList.config.signing repositoryList.newArchitecture repositoryList.newSource repositoryList.force repositoryList.config.server) newMsg repositoryList.list
            in
                ({ repositoryList | list = listModel }, Cmd.batch
                    [ Cmd.map ListMsg listMsg
                    , Cmd.map (ListMsg << Aptly.Generic.List.ItemMsg << Aptly.Published.Repository.SnapshotListMsg) snapshotListMsg
                    ])

        ListMsg (Aptly.Generic.List.State (Aptly.Generic.List.Editing oldRepository newRepository)) ->
            let
                (snapshotListModel, snapshotListMsg) =
                    Aptly.SnapshotList.init repositoryList.config

                newMsg =
                    Aptly.Generic.List.State (Aptly.Generic.List.Editing oldRepository ({ newRepository | snapshotList = Just snapshotListModel }))

                (listModel, listMsg) =
                    Aptly.Generic.List.update Aptly.Published.Repository.update (factory repositoryList.config.signing repositoryList.newArchitecture repositoryList.newSource repositoryList.force repositoryList.config.server) newMsg repositoryList.list
            in
                ({ repositoryList | list = listModel }, Cmd.batch
                    [ Cmd.map ListMsg listMsg
                    , Cmd.map (\msg -> ListMsg <| Aptly.Generic.List.ItemMsg <| Aptly.Published.Repository.SnapshotListMsg msg) snapshotListMsg
                    ])

        ListMsg (Aptly.Generic.List.ItemMsg (Aptly.Published.Repository.SnapshotListMsg (Aptly.SnapshotList.ListModification (Aptly.Generic.List.List (Ok snapshots))))) ->
            let
                (listModel, listMsg) =
                    Aptly.Generic.List.update Aptly.Published.Repository.update (factory repositoryList.config.signing repositoryList.newArchitecture repositoryList.newSource repositoryList.force repositoryList.config.server) (Aptly.Generic.List.ItemMsg <| Aptly.Published.Repository.SnapshotListMsg <| Aptly.SnapshotList.ListModification <| Aptly.Generic.List.List <| Ok snapshots) repositoryList.list
            in
                case List.head snapshots of
                    Nothing ->
                        ({ repositoryList | list = listModel }, Cmd.map ListMsg listMsg)

                    Just firstSnapshot ->
                        ({ repositoryList | list = listModel, newSource = Aptly.Source.Source repositoryList.newSource.component firstSnapshot.name }, Cmd.map ListMsg listMsg)

        ListMsg msg ->
            let
                (listModel, listMsg) =
                    Aptly.Generic.List.update Aptly.Published.Repository.update (factory repositoryList.config.signing repositoryList.newArchitecture repositoryList.newSource repositoryList.force repositoryList.config.server) msg repositoryList.list
            in
                ({ repositoryList | list = listModel }, Cmd.map ListMsg listMsg)

        Force force ->
            ({ repositoryList | force = force }, Cmd.none)

        NewArchitectureChanged newArchitecture ->
            ({ repositoryList | newArchitecture = newArchitecture }, Cmd.none)

        NewSourceChanged newSource ->
            ({ repositoryList | newSource = newSource }, Cmd.none)

view : RepositoryList -> Html.Html Msg
view repositoryList =
    Html.map ListMsg <| Aptly.Generic.List.view (factory repositoryList.config.signing repositoryList.newArchitecture repositoryList.newSource repositoryList.force repositoryList.config.server) (Aptly.Published.Repository.Repository Nothing "" "" "" Aptly.Published.Repository.Snapshot [] [] "" "" Nothing) "Published Repositories" repositoryList.list

subscriptions : RepositoryList -> Sub Msg
subscriptions repositoryList =
    Sub.map ListMsg <| Aptly.Generic.List.subscriptions Aptly.Published.Repository.subscriptions repositoryList.list
