module Debounce exposing
    ( Config
    , Debounce
    , Msg
    , Send
    , Strategy
    , init
    , later
    , manual
    , manualAfter
    , push
    , soon
    , soonAfter
    , takeAll
    , takeLast
    , unlock
    , update
    )

{-| The Debouncer.

Vendored from [jinjor/elm-debounce](https://github.com/jinjor/elm-debounce)
(BSD-3-Clause, © Yosuke Torii), ported to `lamdera/program-test`'s Effect
system: published Elm debounce packages produce `Cmd` via `Process.sleep`
internally, which can't participate in this app's `Command`-based update
functions (or its simulated tests), so the timing primitives are swapped
for `Effect.Process`/`Effect.Task` and delays take `Duration` instead of
`Float` milliseconds. The API and semantics are otherwise unchanged.

  - This module works with the Elm Architecture.
  - You can choose the strategy and define how commands are sent.

-}

import Duration exposing (Duration)
import Effect.Command exposing (Command)
import Effect.Process
import Effect.Task


{-| The state of the debouncer.

It is parameterized with the value type `a`.

-}
type Debounce a
    = Debounce
        { input : List a
        , locked : Bool
        }


length : Debounce a -> Int
length (Debounce { input }) =
    List.length input


{-| Config contains the debouncing strategy and the message transformation.

This config should be constant and shared between the `update` function
and the `push` function.

-}
type alias Config msg =
    { strategy : Strategy
    , transform : Msg -> msg
    }


{-| Strategy defines the timing when commands are sent.
-}
type Strategy
    = Manual Duration
    | Soon Duration Duration
    | Later Duration


{-| Send command as soon as it gets ready, with given rate limit.
(a.k.a. Throttle)

Note: The first command will be sent immediately.

-}
soon : Duration -> Strategy
soon =
    Soon (Duration.milliseconds 0)


{-| Similar to `soon`, but the first command is sent after offset time.
-}
soonAfter : Duration -> Duration -> Strategy
soonAfter =
    Soon


{-| Send command after becoming stable, with given delay time.
(a.k.a. Debounce)
-}
later : Duration -> Strategy
later =
    Later


{-| Send command as soon as it gets ready, but not again until it gets
unlocked manually. See `unlock`.

Typically, `unlock` is called after the previous response comes back.

-}
manual : Strategy
manual =
    Manual (Duration.milliseconds 0)


{-| Similar to `manual`, but the first command is sent after offset time.
-}
manualAfter : Duration -> Strategy
manualAfter =
    Manual


{-| This function consumes values and sends a command.

If you want to postpone sending, return the values back to keep them.

-}
type alias Send a restriction toBackend msg =
    a -> List a -> ( List a, Command restriction toBackend msg )


{-| Send a command using the latest value.
-}
takeLast : (a -> Command restriction toBackend msg) -> Send a restriction toBackend msg
takeLast send head _ =
    ( [], send head )


{-| Send a command using all the accumulated values.
-}
takeAll : (a -> List a -> Command restriction toBackend msg) -> Send a restriction toBackend msg
takeAll send head tail =
    ( [], send head tail )


{-| Initialize the debouncer. Call this from your `init` function.
-}
init : Debounce a
init =
    Debounce
        { input = []
        , locked = False
        }


{-| The messages that are used internally.

(The upstream package also has a `NoOp` constructor; it's dropped here
because nothing ever constructs it.)

-}
type Msg
    = Flush (Maybe Duration)
    | SendIfLengthNotChangedFrom Int


{-| This is the component's update function following the Elm Architecture.

e.g. Saving the last value:

    ( debounce, cmd ) =
        Debounce.update
            { strategy = Debounce.later (Duration.seconds 1)
            , transform = DebounceMsg
            }
            (Debounce.takeLast save)
            -- save : value -> Command restriction toBackend Msg
            msg
            model.debounce

The config should be constant and shared with the `push` function.

The sending logic can depend on the current model. If you want to stop
sending, return `Command.none`.

-}
update : Config msg -> Send a restriction toBackend msg -> Msg -> Debounce a -> ( Debounce a, Command restriction toBackend msg )
update config send msg (Debounce d) =
    case msg of
        Flush tryAgainAfter ->
            case d.input of
                head :: tail ->
                    let
                        ( input, sendCmd ) =
                            send head tail

                        selfCmd =
                            case tryAgainAfter of
                                Just delay ->
                                    delayCmd delay (Flush (Just delay))

                                Nothing ->
                                    Effect.Command.none
                    in
                    ( Debounce
                        { d
                            | input = input
                            , locked = True
                        }
                    , Effect.Command.batch [ sendCmd, mapSelf config selfCmd ]
                    )

                _ ->
                    ( Debounce { d | locked = False }
                    , Effect.Command.none
                    )

        SendIfLengthNotChangedFrom lastInputLength ->
            case ( List.length d.input <= lastInputLength, d.input ) of
                ( True, head :: tail ) ->
                    let
                        ( input, cmd ) =
                            send head tail
                    in
                    ( Debounce { d | input = input }
                    , cmd
                    )

                _ ->
                    ( Debounce d
                    , Effect.Command.none
                    )


{-| Manually unlock. This works for the `manual` or `manualAfter` Strategy.
-}
unlock : Config msg -> Command restriction toBackend msg
unlock config =
    Effect.Task.perform
        config.transform
        (Effect.Task.succeed (Flush Nothing))


{-| Push a value into the debouncer.
-}
push : Config msg -> a -> Debounce a -> ( Debounce a, Command restriction toBackend msg )
push config a (Debounce d) =
    let
        newDebounce =
            Debounce { d | input = a :: d.input }

        selfCmd =
            case config.strategy of
                Manual offset ->
                    if d.locked then
                        Effect.Command.none

                    else
                        delayCmd offset (Flush Nothing)

                Soon offset delay ->
                    if d.locked then
                        Effect.Command.none

                    else
                        delayCmd offset (Flush (Just delay))

                Later delay ->
                    delayCmd delay (SendIfLengthNotChangedFrom (length newDebounce))
    in
    ( newDebounce, mapSelf config selfCmd )


mapSelf : Config msg -> Command restriction toBackend Msg -> Command restriction toBackend msg
mapSelf config cmd =
    Effect.Command.map identity config.transform cmd


delayCmd : Duration -> Msg -> Command restriction toBackend Msg
delayCmd delay msg =
    Effect.Task.perform (\_ -> msg) (Effect.Process.sleep delay)
