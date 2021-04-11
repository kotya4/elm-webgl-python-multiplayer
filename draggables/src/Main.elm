port module Main exposing ( main )

import Browser
import Html exposing ( Html, div, a, text )
import Html.Attributes exposing ( style, href, class, target )
import Html.Events
import Json.Decode
import Random

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

type alias ListUInt16 = List Int

port sendPortMessage    : ( ListUInt16 ) -> Cmd msg
port receivePortMessage : ( ListUInt16 -> msg ) -> Sub msg

type OutcomingPortMessage
  = InitializationRequest
  | DraggableGrabRequest  Int Int Int Int Int
  | DraggablePutRequest   Int
  | DraggableMoveRequest  Int Int Int

encodePortMessage : OutcomingPortMessage -> ListUInt16
encodePortMessage message =
  case message of
    InitializationRequest              -> [ 1                   ]
    DraggableGrabRequest  id x y ox oy -> [ 2, id, x, y, ox, oy ]
    DraggablePutRequest   id           -> [ 3, id               ]
    DraggableMoveRequest  id x y       -> [ 4, id, x, y         ]

encodeAndSendPortMessage : OutcomingPortMessage -> Cmd msg
encodeAndSendPortMessage message =
  encodePortMessage message |> sendPortMessage

type IncomingPortMessage
  = EmptyPortMessage
  | WebSocketOpened
  | WebSocketClosed
  | Initialization   ( List Int )
  | DraggableGrabbed ( List Int )
  | DraggablePutted  ( List Int )
  | DraggableMoved   ( List Int )
  | IncorrectAction

decodePortMessage : ListUInt16 -> IncomingPortMessage
decodePortMessage message =
  case message of
    []          -> EmptyPortMessage
    [ 0      ]  -> WebSocketOpened
    [ 0xffff ]  -> WebSocketClosed
    (act::args) ->
      case act of
        1 -> Initialization   args
        2 -> DraggableGrabbed args
        3 -> DraggablePutted  args
        4 -> DraggableMoved   args
        _ -> IncorrectAction

updateOnPortMessageReceived : ListUInt16 -> Model -> ( Model, Cmd Msg )
updateOnPortMessageReceived message model =
  let
    decodedMessage = decodePortMessage message
    text = stringifyPortMessage decodedMessage
  in
  case decodedMessage of
    EmptyPortMessage ->
      ( { model | text = text }
      , Cmd.none
      )
    WebSocketOpened ->
      ( { model | text = text }
      , encodeAndSendPortMessage InitializationRequest
      )
    WebSocketClosed ->
      ( { model | isWebSocketClosed = True, text = text }
      , Cmd.none
      )
    Initialization (client::raw) ->
      ( { model | client = client, draggables = draggableListFromRaw raw, text = text }
      , Cmd.none
      )
    Initialization _ -> -- Err
      ( model
      , Cmd.none
      )
    DraggableGrabbed raw ->
      case draggableFromRaw raw of
        Just d ->
          ( { model | draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables, text = text }
          , if model.isMouseUp then -- Reduces lag
              encodeAndSendPortMessage ( DraggablePutRequest d.id )
            else
              Cmd.none
          )
        Nothing -> -- Err
          ( model
          , Cmd.none
          )
    DraggablePutted raw ->
      case draggableFromRaw raw of
        Just d ->
          ( { model | draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables, text = text }
          , Cmd.none
          )
        Nothing -> -- Err
          ( model
          , Cmd.none
          )
    DraggableMoved raw ->
      case draggableFromRaw raw of
        Just d ->
          ( { model | draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables, text = text }
          , Cmd.none
          )
        Nothing -> -- Err
          ( model
          , Cmd.none
          )
    IncorrectAction ->
      ( { model | text = text }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = receivePortMessage PortMessageReceived

-- MODEL

type alias Model =
  { text               : String
  , client             : Int
  , isMouseUp          : Bool
  , isWebSocketClosed  : Bool
  , draggables         : List Draggable
  , activeDraggable    : Maybe Draggable
  }
-- TIP: isMouseUp not equal to ( activeDraggable == Nothing )

init : () -> ( Model, Cmd Msg )
init flags =
  ( Model "text" 0 True False [] Nothing
  , Cmd.none
  )

-- UPDATE

type Msg
  = PortMessageReceived    ListUInt16
  | MouseLeave          MousePosition
  | MouseMove           MousePosition
  | MouseUp             MousePosition
  | DraggableMsg     DraggableMessage

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    PortMessageReceived message ->
      updateOnPortMessageReceived message model
    MouseLeave mousePos ->
      case model.activeDraggable of
        Just activeDraggable ->
          updateDraggable ( DraggableMouseUp activeDraggable mousePos ) model
        Nothing ->
          ( model, Cmd.none )
    MouseMove mousePos ->
      case model.activeDraggable of
        Just activeDraggable ->
          updateDraggable ( DraggableMouseMove activeDraggable mousePos ) model
        Nothing ->
          ( model, Cmd.none )
    MouseUp mousePos ->
      case model.activeDraggable of
        Just activeDraggable ->
          updateDraggable ( DraggableMouseUp activeDraggable mousePos ) model
        Nothing ->
          ( model, Cmd.none )
    DraggableMsg dmsg ->
      updateDraggable dmsg model

type DraggableMessage
  = DraggableMouseUp   Draggable MousePosition
  | DraggableMouseDown Draggable MousePosition
  | DraggableMouseMove Draggable MousePosition

updateDraggable : DraggableMessage -> Model -> ( Model, Cmd Msg )
updateDraggable dmsg model =
  case dmsg of
    DraggableMouseMove activeDraggable mousePos ->
      if model.client /= 0 && activeDraggable.owner == model.client && not model.isMouseUp then
        let
          x = mousePos.clientX
          y = mousePos.clientY
          d = { activeDraggable | x = x, y = y }
          draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables
        in
        ( { model | activeDraggable = Just d, draggables = draggables }
        , encodeAndSendPortMessage ( DraggableMoveRequest d.id x y )
        )
      else
        ( model, Cmd.none )
    DraggableMouseDown activeDraggable mousePos ->
      if model.client /= 0 && activeDraggable.owner == 0 then
        let
          x = mousePos.clientX
          y = mousePos.clientY
          offsetX = mousePos.clientX - activeDraggable.x + activeDraggable.offsetX
          offsetY = mousePos.clientY - activeDraggable.y + activeDraggable.offsetY
          d = { activeDraggable | x = x, y = y, offsetX = offsetX, offsetY = offsetY, owner = model.client }
          draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables
        in
        ( { model | activeDraggable = Just d, draggables = draggables, isMouseUp = False }
        , encodeAndSendPortMessage ( DraggableGrabRequest d.id x y offsetX offsetY )
        )
      else
        ( model, Cmd.none )
    DraggableMouseUp activeDraggable mousePos ->
      if model.client /= 0 && activeDraggable.owner == model.client then
        let
          d = { activeDraggable | owner = 0 }
          draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables
        in
        ( { model | activeDraggable = Nothing, draggables = draggables, isMouseUp = True }
        , encodeAndSendPortMessage ( DraggablePutRequest d.id )
        )
      else
        ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  div
    -- Main container
    [ class "container"
    , Html.Events.on "mouseleave" ( Json.Decode.map MouseLeave decodeMousePosition )
    , Html.Events.on "mousemove" ( Json.Decode.map MouseMove decodeMousePosition )
    , Html.Events.on "mouseup" ( Json.Decode.map MouseUp decodeMousePosition )
    ]
    [ div
        -- Debugging messages
        [ class "debug-message"
        ]
        [ text model.text
        ]
    , a
        -- Source link
        [ class "source-link"
        , href   "https://github.com/sluchaynayakotya/-elm/blob/master/draggables"
        , target "_blank"
        ]
        [ text "src"
        ]
    , div
        -- WebSocket connection closed message
        [ class "connection-lost"
        , style "display" ( if model.isWebSocketClosed then "flex" else "none" )
        ]
        [ text "Connection lost. Reload the page"
        ]
    , Html.map ( \ msg -> DraggableMsg msg ) ( viewDraggables model.draggables )
    ]

-- DRAGGABLE

type alias Draggable =
  { id      : Int
  , x       : Int
  , y       : Int
  , offsetX : Int
  , offsetY : Int
  , owner   : Int
  }

draggableFromRaw : List Int -> Maybe Draggable
draggableFromRaw list =
  case list of
    (id::x::y::offsetX::offsetY::owner::_) ->
      Just ( Draggable id x y offsetX offsetY owner )
    _ ->
      Nothing

draggableListFromRaw : List Int -> List Draggable
draggableListFromRaw list =
  case list of
    (id::x::y::offsetX::offsetY::owner::tail) ->
      Draggable id x y offsetX offsetY owner :: draggableListFromRaw tail
    _ -> []

viewDraggablesAsList : List Draggable -> List ( Html DraggableMessage )
viewDraggablesAsList draggables =
  case draggables of
    [] -> []
    (draggable::tail) ->
      div
        [ class "draggable"
        , style "background" ( stringifyRGB ( hashRGB draggable.owner ) )
        , style "z-index" ( String.fromInt ( List.length tail ) )
        , style "left" ( String.fromInt ( draggable.x - draggable.offsetX ) ++ "px" )
        , style "top"  ( String.fromInt ( draggable.y - draggable.offsetY ) ++ "px" )
        , Html.Events.on "mousedown" ( Json.Decode.map ( DraggableMouseDown draggable ) decodeMousePosition )
        ]
        [
        ]
      :: viewDraggablesAsList tail

viewDraggables : List Draggable -> Html DraggableMessage
viewDraggables draggables =
  div
    [ class "container"
    ]
    ( viewDraggablesAsList draggables )

-- HELPERS

stringifyMousePosition : MousePosition -> String
stringifyMousePosition mousePos =
  String.fromInt mousePos.clientX ++ " " ++ String.fromInt mousePos.clientY

stringifyListInt : List Int -> String
stringifyListInt list =
  case list of
    (x::xs) -> String.fromInt x ++ " " ++ stringifyListInt xs
    _       -> ""

stringifyPortMessage : IncomingPortMessage -> String
stringifyPortMessage msg =
  case msg of
    EmptyPortMessage      -> "EmptyPortMessage"
    WebSocketOpened       -> "WebSocketOpened"
    WebSocketClosed       -> "WebSocketClosed"
    Initialization   list -> "Initialization"
    DraggableGrabbed list -> "DraggableGrabbed" ++ stringifyListInt list
    DraggablePutted  list -> "DraggablePutted"  ++ stringifyListInt list
    DraggableMoved   list -> "DraggableMoved"   ++ stringifyListInt list
    IncorrectAction       -> "IncorrectAction"

randomTuple3 : Random.Generator a -> Random.Generator ( a, a, a )
randomTuple3 generator =
  Random.map3 (\ a b c -> ( a, b, c )) generator generator generator

hashRGB : Int -> ( Int, Int, Int )
hashRGB seed =
  let
    seed0 = Random.initialSeed seed
    generator = randomTuple3 ( Random.int 100 255 )
    ( value, _ ) = Random.step generator seed0
  in
  value

stringifyRGB : ( Int, Int, Int ) -> String
stringifyRGB ( r, g, b ) =
  "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"

-- EVENTS AND DECODERS

type alias MousePosition =
  { clientX : Int
  , clientY : Int
  }

decodeMousePosition : Json.Decode.Decoder MousePosition
decodeMousePosition =
  Json.Decode.map2 MousePosition
    ( Json.Decode.at [ "clientX" ] Json.Decode.int )
    ( Json.Decode.at [ "clientY" ] Json.Decode.int )
