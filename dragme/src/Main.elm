module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode
import Debug


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL



type alias Model =
  { msgWas  : String
  , redcube : Redcube
  }
init : Model
init =
  Model "no messages yet" ( Redcube (100, 100) (0, 0) False )


-- UPDATE


type Msg
  = RedcubeDragged MousePosition
  | RedcubePlaced
  | MouseMoved MousePosition


update : Msg -> Model -> Model
update msg model =
  case msg of
    RedcubeDragged origin ->
      let
        orig = subTuples ( tuplefy origin ) model.redcube.position
      in
        { model
        | redcube = drag model.redcube orig
        , msgWas = "RedcubeDragged " ++ ( Tuple.first orig |> String.fromInt ) ++ " " ++ ( Tuple.second orig |> String.fromInt )
        }

    RedcubePlaced ->
      { model
      | redcube = place model.redcube
      , msgWas = "RedcubePlaced"
      }

    MouseMoved position ->
      let
        pos = tuplefy position
      in
        { model
        | redcube = move model.redcube pos
        , msgWas = "MouseMoved " ++ ( Tuple.first pos |> String.fromInt ) ++ " " ++ ( Tuple.second pos |> String.fromInt )
        }




-- VIEW


view : Model -> Html Msg
view model =
  viewMainContainer
  [ div [] [ text model.msgWas ]
  , viewRedcube model.redcube
  ]

viewMainContainer : List ( Html Msg ) -> Html Msg
viewMainContainer contents =
  div
    [ style "margin"         "0"
    , style "padding"        "0"
    , style "background"  "#111"
    , style "color"       "#aaa"
    , style "width"      "100vw"
    , style "height"     "100vh"
    , style "overflow"  "hidden"
    , style "user-select" "none"
    , Html.Events.on "mousemove" <| Json.Decode.map MouseMoved decodeMousePosition
    , Html.Events.on "mouseup"   <| Json.Decode.succeed RedcubePlaced
    ]
    contents





viewRedcube : Redcube -> Html Msg
viewRedcube redcube =
  let
    background =
      case redcube.dragged of
        True  -> "blue"
        False -> "red"
  in
    div
      [ style "width"             "64px"
      , style "height"            "64px"
      , style "position"      "absolute"
      , style "display"           "flex"
      , style "justify-content" "center"
      , style "align-items"     "center"
      , style "color"            "white"
      , style "border-radius"     "10px"
      , style "background" background
      , style "left" <| ( Tuple.first  redcube.position |> String.fromInt ) ++ "px"
      , style "top"  <| ( Tuple.second redcube.position |> String.fromInt ) ++ "px"
      , Html.Events.on "mousedown" <| Json.Decode.map RedcubeDragged decodeMousePosition
      ]
      [ text "Drag Me"
      ]



-- OTHER



type alias Redcube =
  { position : (Int, Int)
  , origin   : (Int, Int)
  , dragged  : Bool
  }

move : Redcube -> (Int, Int) -> Redcube
move redcube position =
  if redcube.dragged == True then
    { redcube
    | position = subTuples position redcube.origin
    }
  else
    redcube

drag : Redcube -> (Int, Int) -> Redcube
drag redcube origin =
  { redcube | dragged = True, origin = origin }

place : Redcube -> Redcube
place redcube =
  { redcube | dragged = False }



type alias MousePosition =
  { clientX : Int
  , clientY : Int
  }


decodeMousePosition : Json.Decode.Decoder MousePosition
decodeMousePosition =
  Json.Decode.map2 MousePosition
    ( Json.Decode.at [ "clientX" ] Json.Decode.int )
    ( Json.Decode.at [ "clientY" ] Json.Decode.int )


tuplefy : MousePosition -> (Int, Int)
tuplefy p =
  ( p.clientX, p.clientY )


--subTuples : (number, number) -> (number, number) -> (number, number)
--subTuples a b =
--  ( Tuple.first a - Tuple.first b, Tuple.second a - Tuple.second b )

subTuples : (number, number) -> (number, number) -> (number, number)
subTuples (x1, y1) (x2, y2) =
  (x1 - x2, y1 - y2)









