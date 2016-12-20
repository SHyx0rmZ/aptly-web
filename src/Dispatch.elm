effect module Dispatch where { subscription = DispatchSub, command = DispatchCmd } exposing (dispatch, events)

import Task exposing (Task)

type DispatchCmd msg
    = Dispatch msg

type DispatchSub msg
    = Event msg

type alias State msg =
    { subs : List (DispatchSub msg)
    }

cmdMap : (a -> b) -> DispatchCmd a -> DispatchCmd b
cmdMap wrapper (Dispatch msg) =
    wrapper msg |> Dispatch

dispatch : (List a -> msg) -> List a -> Cmd msg
dispatch msg data =
    command (Dispatch <| msg data)

events : List a -> (List a -> msg) -> Sub msg
events data msg =
    subscription (Event <| msg data)

init : Task Never (State msg)
init =
    Task.succeed (State [])

onEffects : Platform.Router msg (DispatchSub msg) -> List (DispatchCmd msg) -> List (DispatchSub msg) -> State msg -> Task Never (State msg)
onEffects router cmds subs state =
    let
        _ = Debug.log "cmds" cmds
        _ = Debug.log "subs" subs
        _ = Debug.log "state" state
    in
        case subs of
            [] ->
                case cmds of
                    [] ->
                        Task.succeed state

                    Dispatch msg :: tailCmds ->
                        Platform.sendToApp router msg
                            |> Task.andThen (\_ -> onEffects router tailCmds subs state)

            headSub :: tailSubs ->
                onEffects router cmds tailSubs { state | subs = headSub :: state.subs }


onSelfMsg : Platform.Router msg (DispatchSub msg) -> (DispatchSub msg) -> State msg -> Task Never (State msg)
onSelfMsg router msg state =
    Task.succeed state

subMap : (a -> b) -> DispatchSub a -> DispatchSub b
subMap wrapper (Event msg) =
    wrapper msg |> Event
