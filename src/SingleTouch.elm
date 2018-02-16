-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


effect module SingleTouch
    where { subscription = MySub }
    exposing
        ( onStart
        , onMove
        , onEnd
        , onCancel
        , coordinates
        , downs
        , moves
        , ups
        )

{-| This module exposes functions
to deal with single touch interactions.

The coordinates provided are the ones of the first touch in the
["changedTouches"](https://developer.mozilla.org/en-US/docs/Web/API/TouchEvent) list.
As a consequence, it may behave inconsistently
in case of an accidental multitouch usage.
In case of a need for consistency with potential
unwanted multitouch interactions,
you might want to use the `MultiTouch` module which provides
finer grained control over the processing of the touch event.

@docs onStart, onMove, onEnd, onCancel, coordinates, downs, moves, ups

-}

import Dom.LowLevel as Dom
import Dict
import Html
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Private.Touch
import Process
import Task exposing (Task)
import Touch


{-| Triggered on a "touchstart" event.
-}
onStart : (Touch.Coordinates -> msg) -> Html.Attribute msg
onStart tag =
    on "touchstart" tag


{-| Triggered on a "touchmove" event.
-}
onMove : (Touch.Coordinates -> msg) -> Html.Attribute msg
onMove tag =
    on "touchmove" tag


{-| Triggered on a "touchend" event.
-}
onEnd : (Touch.Coordinates -> msg) -> Html.Attribute msg
onEnd tag =
    on "touchend" tag


{-| Triggered on a "touchcancel" event.
-}
onCancel : (Touch.Coordinates -> msg) -> Html.Attribute msg
onCancel tag =
    on "touchcancel" tag


{-| The decoder used to extract a Coordinates from a JavaScript touch event.
-}
coordinates : Decoder Touch.Coordinates
coordinates =
    Private.Touch.decode
        |> Decode.at [ "changedTouches", "0" ]
        |> Decode.map .coordinates


{-| Subscribe to a "touchstart" event anywhere on the screen.
-}
downs : (Touch.Coordinates -> msg) -> Sub msg
downs tag =
    subscription (MySub "touchstart" tag)


{-| Subscribe to a "touchmove" event anywhere on the screen.
-}
moves : (Touch.Coordinates -> msg) -> Sub msg
moves tag =
    subscription (MySub "touchmove" tag)


{-| Subscribe to a "touchend" event anywhere on the screen.
-}
ups : (Touch.Coordinates -> msg) -> Sub msg
ups tag =
    subscription (MySub "touchend" tag)



-- HELPER FUNCTIONS ##################################################


on : String -> (Touch.Coordinates -> msg) -> Html.Attribute msg
on event tag =
    Decode.map tag coordinates
        |> Events.onWithOptions event Private.Touch.stopOptions



-- SUBSCRIPTIONS


type MySub msg
    = MySub String (Touch.Coordinates -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
    MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
    Dict.Dict String (Watcher msg)


type alias Watcher msg =
    { taggers : List (Touch.Coordinates -> msg)
    , pid : Process.Id
    }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
    Dict.Dict String (List (Touch.Coordinates -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
    categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
    case subs of
        [] ->
            subDict

        (MySub category tagger) :: rest ->
            categorizeHelp rest <|
                Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
    case maybeValues of
        Nothing ->
            Just [ value ]

        Just values ->
            Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
    Task.succeed Dict.empty


type alias Msg =
    { category : String
    , coordinates : Touch.Coordinates
    }


(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
    let
        leftStep category { pid } task =
            Process.kill pid &> task

        bothStep category { pid } taggers task =
            task
                |> Task.andThen (\state -> Task.succeed (Dict.insert category (Watcher taggers pid) state))

        rightStep category taggers task =
            let
                tracker =
                    Dom.onDocument category coordinates (Platform.sendToSelf router << Msg category)
            in
                task
                    |> Task.andThen
                        (\state ->
                            Process.spawn tracker
                                |> Task.andThen (\pid -> Task.succeed (Dict.insert category (Watcher taggers pid) state))
                        )
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            oldState
            (categorize newSubs)
            (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router { category, coordinates } state =
    case Dict.get category state of
        Nothing ->
            Task.succeed state

        Just { taggers } ->
            let
                send tagger =
                    Platform.sendToApp router (tagger coordinates)
            in
                Task.sequence (List.map send taggers)
                    &> Task.succeed state
