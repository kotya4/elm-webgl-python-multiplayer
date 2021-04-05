port module Draggable exposing ( main )

import Browser
import Html exposing ( Html, div, text )
import Html.Attributes exposing ( style )
import Html.Events
import Json.Decode
import Bitwise
import Random
import Debug

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }

-- PORTS

type alias IncomingPortMessage  = List Int
type alias OutcomingPortMessage = List Int

port sendPortMessage    : OutcomingPortMessage -> Cmd msg
port receivePortMessage : ( IncomingPortMessage -> msg ) -> Sub msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = receivePortMessage PortMessageReceived

-- MODEL

type alias Model =
  { text : String
  }

init : () -> ( Model, Cmd Msg )
init flags =
  ( Model "text"
  , Cmd.none
  )

-- UPDATE

type Msg
  = Nothing
  | PortMessageReceived IncomingPortMessage

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    Nothing ->
      ( model, Cmd.none )

    PortMessageReceived _ ->
      ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  div
    -- Main container
    [ style "width"       "100vw"
    , style "height"      "100vh"
    , style "overflow"   "hidden"
    , style "position" "absolute"
    , style "left"            "0"
    , style "top"             "0"
    --, Html.Events.on "mousemove" <| Json.Decode.map MouseMove decodeMousePosition
    --, Html.Events.on "mouseup"   <| Json.Decode.map MouseUp   decodeMousePosition
    ]
    [ div
        -- Debugging messages
        [ style "position" "absolute"
        , style "bottom"          "0"
        , style "left"            "0"
        ]
        [ text model.text
        ]
    ]
