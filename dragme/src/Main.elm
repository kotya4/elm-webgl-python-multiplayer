module Main exposing (main)

import Debug
import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode
import Random


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL



type alias Model =
  { msgWas  :  String
  , redcube : Redcube
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( Model "no messages yet" ( Redcube (100, 100) (0, 0) False "Grab me" )
  , Random.generate RedcubePlace randomPointOnScreen
  )


randomPointOnScreen : Random.Generator (Int, Int)
randomPointOnScreen =
  Random.pair ( Random.int 0 500 ) ( Random.int 0 500 )


-- UPDATE


type Msg
  = RedcubeGrab        MousePosition
  | RedcubePlace        ( Int, Int )
  | MouseUp            MousePosition
  | MouseMove          MousePosition
  | MouseDown          MousePosition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RedcubeGrab position ->
      let
        pos  = tuplefyMousePosition position
        orig = subTuples pos model.redcube.position
      in
      ( { model
            | redcube = drag model.redcube orig
            , msgWas  = "RedcubeGrab " ++ stringifyTuple pos
        }
      , Cmd.none
      )

    RedcubePlace pos ->
      ( { model
            | redcube = place <| move model.redcube pos
            , msgWas  = "RedcubePlace"
        }
      , Cmd.none
      )

    MouseUp position ->
      case model.redcube.dragged of
        True ->
          let
            pos = tuplefyMousePosition position
          in
          update ( RedcubePlace pos ) model
        _    ->
          ( model
          , Cmd.none
          )

    MouseMove position ->
      case model.redcube.dragged of
        True ->
          let
            pos = tuplefyMousePosition position
          in
          ( { model
                | redcube = move model.redcube pos
                , msgWas = "MouseMove " ++ stringifyTuple pos
            }
          , Cmd.none
          )
        _    ->
          ( model
          , Cmd.none
          )

    MouseDown _ ->
      ( model
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  viewMainContainer
  --[ div [] [ text model.msgWas ]
  [ viewRedcube model.redcube
  ]



-- VIEW STUFF



viewMainContainer : List ( Html Msg ) -> Html Msg
viewMainContainer contents =
  div
    --[ style "margin"         "0"
    --, style "padding"        "0"
    --, style "background"  "#111"
    --, style "color"       "#aaa"
    --, style "user-select" "none"
    [ style "width"       "100vw"
    , style "height"      "100vh"
    , style "overflow"   "hidden"
    , style "position" "absolute"
    , style "left"            "0"
    , style "top"             "0"
    , Html.Events.on "mousemove" <| Json.Decode.map MouseMove decodeMousePosition
    , Html.Events.on "mouseup"   <| Json.Decode.map MouseUp   decodeMousePosition
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
      , style "user-select"       "none"
      , style "background" background
      , style "left" <| ( Tuple.first  redcube.position |> String.fromInt ) ++ "px"
      , style "top"  <| ( Tuple.second redcube.position |> String.fromInt ) ++ "px"
      , Html.Events.on "mousedown" <| Json.Decode.map RedcubeGrab decodeMousePosition
      ]
      [ text redcube.text
      ]



-- PLAYGROUND



stringifyTuple : (Int, Int) -> String
stringifyTuple (a, b) =
  String.fromInt a ++ " " ++ String.fromInt b



type alias Redcube =
  { position : (Int, Int)
  , origin   : (Int, Int)
  , dragged  : Bool
  , text     : String
  }

move : Redcube -> (Int, Int) -> Redcube
move redcube position =
  { redcube | position = subTuples position redcube.origin }

drag : Redcube -> (Int, Int) -> Redcube
drag redcube origin =
  { redcube | dragged = True, origin = origin, text = "Put me" }

place : Redcube -> Redcube
place redcube =
  { redcube | dragged = False, text = "Grab me" }



type alias MousePosition =
  { clientX : Int
  , clientY : Int
  }


decodeMousePosition : Json.Decode.Decoder MousePosition
decodeMousePosition =
  Json.Decode.map2 MousePosition
    ( Json.Decode.at [ "clientX" ] Json.Decode.int )
    ( Json.Decode.at [ "clientY" ] Json.Decode.int )


tuplefyMousePosition : MousePosition -> (Int, Int)
tuplefyMousePosition p =
  ( p.clientX, p.clientY )


--subTuples : (number, number) -> (number, number) -> (number, number)
--subTuples a b =
--  ( Tuple.first a - Tuple.first b, Tuple.second a - Tuple.second b )

subTuples : (number, number) -> (number, number) -> (number, number)
subTuples (x1, y1) (x2, y2) =
  (x1 - x2, y1 - y2)









