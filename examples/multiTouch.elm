module Main exposing (..)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Touch exposing (TouchEvent(..), Touch)
import MultiTouch exposing (MultiTouch, onMultiTouch)


main =
    H.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL #############################################################


type alias Model =
    { multiTouch : Maybe MultiTouch
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, Cmd.none )



-- UPDATE ############################################################


type Msg
    = MultiTouchMsg MultiTouch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MultiTouchMsg multiTouch ->
            ( { model | multiTouch = Just multiTouch }
            , Cmd.none
            )



-- VIEW ##############################################################


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.p [] [ H.text "Try to touch the image below and watch the model changing." ]
        , H.p [] [ H.text "(Only works on mobile platforms)" ]
        , H.img ([ HA.src "http://lorempixel.com/600/300/" ] ++ onAllMultiTouch) []
        , H.p [] [ H.text <| toString model ]
        ]


onAllMultiTouch : List (H.Attribute Msg)
onAllMultiTouch =
    [ onMultiTouch TouchStart Touch.preventAndStop MultiTouchMsg
    , onMultiTouch TouchMove Touch.preventAndStop MultiTouchMsg
    , onMultiTouch TouchEnd Touch.preventAndStop MultiTouchMsg
    , onMultiTouch TouchCancel Touch.preventAndStop MultiTouchMsg
    ]
