effect module Aptly.Local.RepositoryListSynchronizer where { subscription = MySub, command = MyCmd } exposing (modify, onModify)

import Aptly.Generic.List
import Aptly.Local.Repository
import Task exposing (Task)

type MyCmd msg
    = Modify (Aptly.Generic.List.Modification Aptly.Local.Repository.Repository)

type MySub msg
    = OnModify (Aptly.Generic.List.Modification Aptly.Local.Repository.Repository -> msg)

type alias State msg =
    { subscriptions : List (MySub msg)
    }

type SelfMsg
    = Modification (Aptly.Generic.List.Modification Aptly.Local.Repository.Repository)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap tagger (Modify modification) =
    Modify modification

modify : Aptly.Generic.List.Modification Aptly.Local.Repository.Repository -> Cmd msg
modify data =
    command <| Modify data

onModify : (Aptly.Generic.List.Modification Aptly.Local.Repository.Repository -> msg) -> Sub msg
onModify tagger =
    subscription <| OnModify tagger

init : Task Never (State msg)
init =
    Task.succeed (State [])

onEffects : Platform.Router msg SelfMsg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router commands subscriptions state =
    commands
        |> List.map (\(Modify modification) -> Modification modification)
        |> List.map (Platform.sendToSelf router)
        |> Task.sequence
        |> Task.andThen (\_ -> Task.succeed { state | subscriptions = subscriptions })

onSelfMsg : Platform.Router msg SelfMsg -> SelfMsg -> State msg -> Task Never (State msg)
onSelfMsg router msg state =
    case msg of
        Modification modification ->
            state.subscriptions
                |> List.map (\(OnModify tagger) -> tagger modification)
                |> List.map (Platform.sendToApp router)
                |> Task.sequence
                |> Task.andThen (\_ -> Task.succeed state)

subMap : (a -> b) -> MySub a -> MySub b
subMap higherTagger (OnModify tagger) =
    OnModify <| tagger >> higherTagger
