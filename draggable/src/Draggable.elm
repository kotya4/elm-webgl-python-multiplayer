port module Draggable exposing ( main )

import Browser
import Html exposing ( Html, div, a, text )
import Html.Attributes exposing ( style, href, class, target )
import Html.Events
import Json.Decode
import Bitwise
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

type alias RawOutcomingPortMessage = List Int -- [ action, offsets, y, x ]
type alias RawIncomingPortMessage  = List Int -- [ extra:action, owner, offsets, y, x ]

port sendPortMessage    : RawOutcomingPortMessage -> Cmd msg
port receivePortMessage : ( RawIncomingPortMessage -> msg ) -> Sub msg

type OutcomingPortMessage
  = LoadDraggable
  | GrabDraggable Int Int Int
  | PutDraggable
  | MoveDraggable Int Int

encodePortMessage : OutcomingPortMessage -> RawOutcomingPortMessage
encodePortMessage message =
  case message of
    LoadDraggable             -> [ 1,       0, 0, 0 ]
    GrabDraggable offsets y x -> [ 2, offsets, y, x ]
    PutDraggable              -> [ 3,       0, 0, 0 ]
    MoveDraggable         y x -> [ 4,       0, y, x ]

encodeAndSendPortMessage : OutcomingPortMessage -> Cmd msg
encodeAndSendPortMessage message = encodePortMessage message |> sendPortMessage

type IncomingPortMessage
  = WebSocketReady
  | DraggableLoaded            Int Int Int Int Int Int
  | DraggableGrabbed           Int Int Int Int Int
  | DraggablePutted            Int Int Int Int Int
  | DraggableMoved             Int Int Int Int Int
  | IncorrectPortMessageAction RawIncomingPortMessage
  | IncorrectPortMessageArray  RawIncomingPortMessage

decodePortMessage : RawIncomingPortMessage -> IncomingPortMessage
decodePortMessage message =
  case message of
    [ extraAndAction, owner, offsets, y, x ] ->
      let
        extra   = Bitwise.shiftRightBy 8 extraAndAction
        action  = Bitwise.and 0xff extraAndAction
        offsetY = Bitwise.shiftRightBy 8 offsets
        offsetX = Bitwise.and 0xff offsets
      in
      case action of
        0 -> WebSocketReady
        1 -> DraggableLoaded  extra owner offsetY offsetX y x
        2 -> DraggableGrabbed       owner offsetY offsetX y x
        3 -> DraggablePutted        owner offsetY offsetX y x
        4 -> DraggableMoved         owner offsetY offsetX y x
        _ -> IncorrectPortMessageAction message
    _ -> IncorrectPortMessageArray message

updateOnPortMessageReceived : RawIncomingPortMessage -> Model -> ( Model, Cmd Msg )
updateOnPortMessageReceived message model =
  -- TODO: Can merge messages together
  let
    decodedMessage = decodePortMessage message
    text = stringifyIncomingPortMessage decodedMessage
  in
  case decodedMessage of
    WebSocketReady ->
      ( { model | text = text }, encodeAndSendPortMessage LoadDraggable )

    DraggableLoaded client owner offsetY offsetX y x ->
      let
        oldDraggable = model.draggable
        draggable    = { oldDraggable | owner = owner, offsetY = offsetY, offsetX = offsetX, y = y, x = x }
      in
      ( { model | text = text, draggable = draggable, client = client }, Cmd.none )

    DraggableGrabbed owner offsetY offsetX y x ->
      let
        oldDraggable = model.draggable
        draggable    = { oldDraggable | owner = owner, offsetY = offsetY, offsetX = offsetX, y = y, x = x }
      in
      ( { model | text = text, draggable = draggable }, Cmd.none )

    DraggablePutted owner offsetY offsetX y x ->
      let
        oldDraggable = model.draggable
        draggable    = { oldDraggable | owner = owner, offsetY = offsetY, offsetX = offsetX, y = y, x = x }
        -- TIP: Also can do as: draggable = { oldDraggable | owner = 0, offsetY = 0, offsetX = 0, y = y - offsetY, x = x - offsetX }
      in
      ( { model | text = text, draggable = draggable }, Cmd.none )

    DraggableMoved owner offsetY offsetX y x ->
      let
        oldDraggable = model.draggable
        draggable    = { oldDraggable | owner = owner, offsetY = offsetY, offsetX = offsetX, y = y, x = x }
      in
      ( { model | text = text, draggable = draggable }, Cmd.none )

    IncorrectPortMessageAction _ ->
      ( { model | text = text }, Cmd.none )

    IncorrectPortMessageArray  _ ->
      ( { model | text = text }, Cmd.none )

stringifyIncomingPortMessage : IncomingPortMessage -> String
stringifyIncomingPortMessage message =
  case message of
    WebSocketReady                         -> "WebSocketReady"
    DraggableLoaded client owner oy ox y x -> "DraggableLoaded "  ++ stringifyListInt [ client, owner, oy, ox, y, x ]
    DraggableGrabbed       owner oy ox y x -> "DraggableGrabbed " ++ stringifyListInt [         owner, oy, ox, y, x ]
    DraggablePutted        owner oy ox y x -> "DraggablePutted "  ++ stringifyListInt [         owner, oy, ox, y, x ]
    DraggableMoved         owner oy ox y x -> "DraggableMoved "   ++ stringifyListInt [         owner, oy, ox, y, x ]
    IncorrectPortMessageAction raw         -> "IncorrectPortMessageAction " ++ stringifyListInt raw
    IncorrectPortMessageArray  raw         -> "IncorrectPortMessageArray "  ++ stringifyListInt raw

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = receivePortMessage PortMessageReceived

-- MODEL

type alias Draggable =
  { x       : Int
  , y       : Int
  , owner   : Int
  , offsetX : Int
  , offsetY : Int
  }

type alias Model =
  { text      : String
  , draggable : Draggable
  , client    : Int
  }

init : () -> ( Model, Cmd Msg )
init flags =
  ( Model "text" ( Draggable 0 0 0 0 0 ) 0
  , Cmd.none
  )

isDraggableGrabbed : Model -> Bool
isDraggableGrabbed model =
  model.draggable.owner == model.client && model.client /= 0

isDraggableGrabbable : Model -> Bool
isDraggableGrabbable model =
  model.draggable.owner == 0 && model.client /= 0

-- UPDATE

type Msg
  = PortMessageReceived RawIncomingPortMessage
  | MouseUp             MousePosition
  | MouseMove           MousePosition
  | DraggableMouseDown  MousePosition
  | DraggableMouseUp    MousePosition
  | DraggableMouseMove  MousePosition

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    -- General messages

    PortMessageReceived message ->
      updateOnPortMessageReceived message model

    -- Main container events

    MouseUp mousePos ->
      update ( DraggableMouseUp mousePos ) model

    MouseMove mousePos ->
      update ( DraggableMouseMove mousePos ) model

    -- Draggable events

    DraggableMouseDown mousePos ->
      case isDraggableGrabbable model of
        True ->
          let
            offsetY = Bitwise.and 0xff ( mousePos.clientY - model.draggable.y + model.draggable.offsetY )
            offsetX = Bitwise.and 0xff ( mousePos.clientX - model.draggable.x + model.draggable.offsetX )
            offsets = Bitwise.or ( Bitwise.shiftLeftBy 8 offsetY ) offsetX
            y = mousePos.clientY
            x = mousePos.clientX
          in
          ( model
          , encodeAndSendPortMessage ( GrabDraggable offsets y x )
          )
        False ->
          ( model, Cmd.none )

    DraggableMouseUp _ ->
      case isDraggableGrabbed model of
        True ->
          ( model
          , encodeAndSendPortMessage ( PutDraggable )
          )
        False ->
          ( model, Cmd.none )

    DraggableMouseMove mousePos ->
      case isDraggableGrabbed model of
        True ->
          let
            y = mousePos.clientY
            x = mousePos.clientX
            oldDraggable = model.draggable
            draggable    = { oldDraggable | y = y, x = x }
          in
          ( { model | draggable = draggable }
          , encodeAndSendPortMessage ( MoveDraggable y x )
          )
        False ->
          ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  let
    draggableX = String.fromInt ( model.draggable.x - model.draggable.offsetX ) ++ "px"
    draggableY = String.fromInt ( model.draggable.y - model.draggable.offsetY ) ++ "px"
    draggableColor = chooseDraggableColor model
    draggableText = (\_ ->
      case model.draggable.owner of
        0 -> "Grab me"
        _ -> ""
      ) ()
  in
  div
    -- Main container
    [ style "width"       "100vw"
    , style "height"      "100vh"
    , style "overflow"   "hidden"
    , style "position" "absolute"
    , style "left"            "0"
    , style "top"             "0"
    , Html.Events.on "mousemove" ( Json.Decode.map MouseMove decodeMousePosition )
    , Html.Events.on "mouseup" ( Json.Decode.map MouseUp decodeMousePosition )
    ]
    [ div
        -- Debugging messages
        [ style "position" "absolute"
        , style "bottom"          "0"
        , style "left"            "0"
        ]
        [ text model.text
        ]
        -- Source link
    , a
        [ href   "https://github.com/sluchaynayakotya/-elm/blob/master/draggable/src/Draggable.elm"
        , target "_blank"
        , style  "position" "absolute"
        , style  "top"             "0"
        , style  "right"         "5px"
        ]
        [ text "source"
        ]
    , div
        -- Draggable element
        [ class "draggable"
        , style "left"           draggableX
        , style "top"            draggableY
        , style "background" draggableColor
        , Html.Events.on "mousedown" ( Json.Decode.map DraggableMouseDown decodeMousePosition )
        ]
        [ text draggableText
        ]
    ]

-- TODO: random color generator
chooseDraggableColor : Model -> String
chooseDraggableColor model =
  let
    ownedByClient = model.client /= 0 && model.client == model.draggable.owner
    unaccessable  = model.client == 0
    isFree        = model.draggable.owner == 0
  in
  if ownedByClient then
    "#FF5733"
  else if unaccessable then
    "#C8C8C8"
  else if isFree then
    "white"
  else -- in use
    "#749C82"

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

-- HELPERS

stringifyMousePosition : MousePosition -> String
stringifyMousePosition mousePos =
  String.fromInt mousePos.clientX ++ " " ++ String.fromInt mousePos.clientY

stringifyListInt : List Int -> String
stringifyListInt list =
  case list of
    (x::xs) -> String.fromInt x ++ " " ++ stringifyListInt xs
    []      -> ""
