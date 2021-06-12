port module Main exposing ( main )

import Task
import Browser
import Browser.Dom
import Html exposing ( Html, div, a, text, img, input )
import Html.Attributes exposing ( style, href, class, target, id, src )
import Html.Events
import Json.Decode
import Json.Encode
import Random
import Bitwise
import Time












-- PORTS

type alias ListUInt16 = List Int

port sendWebSocket    : ListUInt16 -> Cmd msg

port receiveWebSocket : ( ListUInt16 -> msg ) -> Sub msg

port receiveWindowSize : ( ( Int, Int ) -> msg ) -> Sub msg

port playSound : String -> Cmd msg

port setSoundVolume : Float -> Cmd msg

port sendLocalStorage : String -> Cmd msg



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ receiveWebSocket WebSocketReceived
  , receiveWindowSize WindowSizeReceived
  , Time.every 1000 Tick
  ]






encodeLocalStorage : Model -> String
encodeLocalStorage { soundVolume, screenRotation } =
  Json.Encode.encode 0 (
    Json.Encode.object
      [ ( "soundVolume", Json.Encode.float soundVolume )
      , ( "screenRotation", Json.Encode.float screenRotation )
      ]
  )








-- WEBSOCKET

type OutcomingWebSocket
  = InitializationRequest
  | DraggableGrabRequest  Int Int Int Int Int
  | DraggablePutRequest   Int Int Int
  | DraggableMoveRequest  Int Int Int

encodeWebSocket : OutcomingWebSocket -> ListUInt16
encodeWebSocket message =
  case message of
    InitializationRequest              -> [ 1                   ]
    DraggableGrabRequest  id x y ox oy -> [ 2, id, x, y, ox, oy ]
    DraggablePutRequest   id x y       -> [ 3, id, x, y         ]
    DraggableMoveRequest  id x y       -> [ 4, id, x, y         ]

encodeAndSendWebSocket : OutcomingWebSocket -> Cmd msg
encodeAndSendWebSocket message =
  encodeWebSocket message |> sendWebSocket

type IncomingWebSocket
  = EmptyPortMessage
  | WebSocketOpened
  | WebSocketClosed
  | Initialization   ( List Int )
  | DraggableGrabbed ( List Int )
  | DraggablePutted  ( List Int )
  | DraggableMoved   ( List Int )
  | IncorrectAction

decodeWebSocket : ListUInt16 -> IncomingWebSocket
decodeWebSocket message =
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

updateOnWebSocketReceived : ListUInt16 -> Model -> ( Model, Cmd Msg )
updateOnWebSocketReceived message model =
  let
    decodedMessage = decodeWebSocket message
    text = stringifyWebSocket decodedMessage
  in
  case decodedMessage of
    EmptyPortMessage ->
      ( { model | text = text }
      , Cmd.none
      )
    WebSocketOpened ->
      ( { model | text = text }
      , encodeAndSendWebSocket InitializationRequest
      )
    WebSocketClosed ->
      ( { model | isWebSocketClosed = True, text = text }
      , Cmd.none
      )
    Initialization (client::lastTimeTail::lastTimeHead::lastOwner::raw) ->
      let
        lastTime = Bitwise.or ( Bitwise.shiftLeftBy 16 lastTimeHead ) lastTimeTail
        draggables = List.map (\a -> { a | depth = 0 }) ( List.reverse ( List.sortBy .depth ( draggableListFromRaw raw ) ) )
      in
      ( { model | client = client, lastTime = lastTime, lastOwner = lastOwner, draggables = draggables, text = text }
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
          , playSound "press.mp3"
          )
        Nothing -> -- Err
          ( model
          , Cmd.none
          )
    DraggablePutted (lastTimeTail::lastTimeHead::lastOwner::raw) ->
      case draggableFromRaw raw of
        Just d ->
          let
            lastTime = Bitwise.or ( Bitwise.shiftLeftBy 16 lastTimeHead ) lastTimeTail
          in
          ( { model | lastTime = lastTime, lastOwner = lastOwner, draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables, text = text }
          , playSound "release.mp3"
          )
        Nothing -> -- Err
          ( model
          , Cmd.none
          )
    DraggablePutted _ -> -- Err
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















-- MODEL

type alias Model =
  { text               : String
  , client             : Int
  , isMouseUp          : Bool
  , isWebSocketClosed  : Bool
  , draggables         : List Draggable
  , activeDraggable    : Maybe Draggable
  , innerWidth         : Int
  , innerHeight        : Int
  , containerX         : Float
  , containerY         : Float
  , containerWidth     : Int
  , containerHeight    : Int
  , scalerX            : Float
  , scalerY            : Float
  , isPageMirrored     : Bool
  , isSettingsShown    : Bool
  , soundVolume        : Float
  , screenRotation     : Float
  , lastTime           : Int
  , lastOwner          : Int
  , lastDraggable      : Maybe Draggable
  , time               : Time.Posix
  }

-- Know that "isMouseUp" not equal to "( activeDraggable == Nothing )".



type alias Flags =
  { localStorage : String
  }


decodeFlags : Json.Decode.Decoder Flags
decodeFlags =
  Json.Decode.map Flags
    ( Json.Decode.at [ "localStorage" ] Json.Decode.string )



type alias LocalStorage =
  { soundVolume    : Float
  , screenRotation : Float
  }

decodeLocalStorage : Json.Decode.Decoder LocalStorage
decodeLocalStorage =
  Json.Decode.map2 LocalStorage
    ( Json.Decode.at [ "soundVolume"    ] Json.Decode.float )
    ( Json.Decode.at [ "screenRotation" ] Json.Decode.float )



init : Json.Decode.Value -> ( Model, Cmd Msg )
init json =
  let

    flags = case Json.Decode.decodeValue decodeFlags json of
      Ok v -> v
      Err e ->
        Flags ""

    localStorage = case Json.Decode.decodeString decodeLocalStorage flags.localStorage of
      Ok v -> v
      Err e ->
        LocalStorage 0 0

    soundVolume    = localStorage.soundVolume
    screenRotation = localStorage.screenRotation

  in
  ( Model "text" 0 True False [] Nothing 0 0 0 0 1000 1000 0 0 False False soundVolume screenRotation 0 0 Nothing ( Time.millisToPosix 0 )
  , Cmd.batch
      [ getContainerElement
      , setSoundVolume soundVolume
      , Task.perform Tick Time.now
      ]
  )


main : Program Json.Decode.Value Model Msg
main =
  Browser.element
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }





















-- UPDATE

type Msg
  = WebSocketReceived      ListUInt16
  | WindowSizeReceived     ( Int, Int )
  | ContainerElementGot    ( Result Browser.Dom.Error Browser.Dom.Element )
  | MouseLeave             MousePosition
  | MouseMove              MousePosition
  | MouseUp                MousePosition
  | DraggableMouseUp       Draggable MousePosition
  | DraggableMouseDown     Draggable MousePosition
  | DraggableMouseMove     Draggable MousePosition
  | DraggableTouchUp     Draggable
  | MirrorThePage          MousePosition
  | ShowSettings           MousePosition
  | SetSoundVolume         String
  | RotateScreen90deg      RotateDirection
  | ShowLastDraggable      ( Maybe Draggable )
  | Tick                   Time.Posix
  | TouchUp                

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    -- Ports

    WebSocketReceived message ->
      updateOnWebSocketReceived message model

    WindowSizeReceived ( innerWidth, innerHeight ) ->
      let
        size = toFloat ( min innerWidth innerHeight )
        scalerX = size / toFloat model.containerWidth
        scalerY = size / toFloat model.containerHeight
      in
      ( { model | innerWidth = innerWidth, innerHeight = innerHeight, scalerX = scalerX, scalerY = scalerY }, getContainerElement )

    -- General

    ContainerElementGot result ->
      case result of
        Err error ->
          ( model, Cmd.none )
        Ok element ->
          let
            containerX = element.element.x / model.scalerX
            containerY = element.element.y / model.scalerY
          in
          ( { model | containerX = containerX, containerY = containerY }, Cmd.none )

    MouseLeave mousePos ->
      case model.activeDraggable of
        Just activeDraggable ->
          update ( DraggableMouseUp activeDraggable mousePos ) model
        Nothing ->
          ( model, Cmd.none )

    MouseMove mousePos ->
      case model.activeDraggable of
        Just activeDraggable ->
          update ( DraggableMouseMove activeDraggable mousePos ) model
        Nothing ->
          ( model, Cmd.none )

    MouseUp mousePos ->
      case model.activeDraggable of
        Just activeDraggable ->
          update ( DraggableMouseUp activeDraggable mousePos ) model
        Nothing ->
          ( model, Cmd.none )

    TouchUp ->
      case model.activeDraggable of
        Just activeDraggable ->
          update ( DraggableTouchUp activeDraggable ) model
        Nothing ->
          ( model, Cmd.none )

    -- Draggable

    DraggableMouseMove activeDraggable rawMousePos ->
      if model.client /= 0 && activeDraggable.owner == model.client && not model.isMouseUp then
        let
          mousePos = rotateMousePosition model.screenRotation ( sreenCenter model.innerWidth model.innerHeight ) rawMousePos
          x = truncate ( mousePos.clientX / model.scalerX - model.containerX )
          y = truncate ( mousePos.clientY / model.scalerY - model.containerY )
          depth = case List.head model.draggables of -- Active draggable have to have highest depth.
            Just head -> head.depth + 1              -- Head of draggables has the highest depth.
            Nothing -> 0                             -- This never happens tho.
          d = { activeDraggable | x = x, y = y, depth = depth }
          draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables
        in
        ( { model | activeDraggable = Just d, draggables = draggables }
        , encodeAndSendWebSocket ( DraggableMoveRequest d.id x y )
        )
      else
        ( model, Cmd.none )

    DraggableMouseDown activeDraggable rawMousePos ->
      if model.client /= 0 && activeDraggable.owner == 0 then
        let
          mousePos = rotateMousePosition model.screenRotation ( sreenCenter model.innerWidth model.innerHeight ) rawMousePos
          x = truncate ( mousePos.clientX / model.scalerX - model.containerX )
          y = truncate ( mousePos.clientY / model.scalerY - model.containerY )
          offsetX = truncate ( mousePos.clientX / model.scalerX - model.containerX ) - activeDraggable.x + activeDraggable.offsetX
          offsetY = truncate ( mousePos.clientY / model.scalerX - model.containerY ) - activeDraggable.y + activeDraggable.offsetY
          depth = case List.head model.draggables of -- Active draggable have to have highest depth.
            Just head -> head.depth + 1              -- Head of draggables has the highest depth.
            Nothing -> 0                             -- This never happens tho.
          d = { activeDraggable | x = x, y = y, offsetX = offsetX, offsetY = offsetY, owner = model.client, depth = depth }
          draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables
        in
        ( { model
            | activeDraggable = Just d
            , draggables = draggables
            , isMouseUp = False
            , lastDraggable = Nothing
          }
        , Cmd.batch
            [ encodeAndSendWebSocket ( DraggableGrabRequest d.id x y offsetX offsetY )
            , playSound "press.mp3"
            ]
        )
      else
        ( model, Cmd.none )

    DraggableMouseUp activeDraggable rawMousePos ->
      if model.client /= 0 && activeDraggable.owner == model.client then
        let
          mousePos = rotateMousePosition model.screenRotation ( sreenCenter model.innerWidth model.innerHeight ) rawMousePos
          x = truncate ( mousePos.clientX / model.scalerX - model.containerX )
          y = truncate ( mousePos.clientY / model.scalerY - model.containerY )
          d = { activeDraggable | owner = 0 }
          draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables
        in
        ( { model
            | activeDraggable = Nothing
            , draggables = draggables
            , isMouseUp = True
            , lastOwner = model.client
            , lastTime = Time.posixToMillis model.time // 1000
          }
        , Cmd.batch
            [ encodeAndSendWebSocket ( DraggablePutRequest d.id x y )
            , playSound "release.mp3"
            ]
        )
      else
        ( model, Cmd.none )



    DraggableTouchUp activeDraggable ->
      if model.client /= 0 && activeDraggable.owner == model.client then
        let
          x = activeDraggable.x
          y = activeDraggable.y
          d = { activeDraggable | owner = 0 }
          draggables = d :: List.filter (\a -> a.id /= d.id) model.draggables
        in
        ( { model
            | activeDraggable = Nothing
            , draggables = draggables
            , isMouseUp = True
            , lastOwner = model.client
            , lastTime = Time.posixToMillis model.time // 1000
          }
        , Cmd.batch
            [ encodeAndSendWebSocket ( DraggablePutRequest d.id x y )
            , playSound "release.mp3"
            ]
        )
      else
        ( model, Cmd.none )














    -- Common

    MirrorThePage _ ->
      ( { model | isPageMirrored = not model.isPageMirrored }, Cmd.none )

    ShowSettings _ ->
      ( { model | isSettingsShown = not model.isSettingsShown }, Cmd.none )

    SetSoundVolume soundVolumeAsString ->
      let

        soundVolume = case String.toFloat soundVolumeAsString of
          Nothing -> 0
          Just v -> v / 100

        newModel = { model | soundVolume = soundVolume }

      in
      ( newModel
      , Cmd.batch
          [ setSoundVolume soundVolume
          , playSound "release.mp3"
          , sendLocalStorage ( encodeLocalStorage newModel )
          ]
      )

    RotateScreen90deg direction ->
      let

        screenRotation = case direction of
          Clockwise     -> model.screenRotation + pi * 0.5
          Anticlockwise -> model.screenRotation - pi * 0.5
          Default       -> 0

        newModel = { model | screenRotation = screenRotation }

      in
      ( newModel, sendLocalStorage ( encodeLocalStorage newModel ) )

    ShowLastDraggable maybeDraggable ->
      ( { model | lastDraggable = maybeDraggable }, Cmd.none )

    Tick time ->
      ( { model | time = time }, Cmd.none )







lastDraggableOf : List Draggable -> Maybe Draggable
lastDraggableOf draggables =
  case draggables of
    [] -> Nothing
    (head::tail) ->
      case tail of
        [] -> Just head
        _ -> lastDraggableOf tail





reverseMousePosition : MousePosition -> Int -> Int -> MousePosition
reverseMousePosition mousePos innerWidth innerHeight =
  { mousePos | clientX = toFloat ( innerWidth - 1 ) - mousePos.clientX, clientY = toFloat ( innerHeight - 1 ) - mousePos.clientY }



type alias Origin
  = ( Float, Float )


sreenCenter : Int -> Int -> Origin
sreenCenter innerWidth innerHeight =
  ( toFloat innerWidth  / 2
  , toFloat innerHeight / 2
  )



rotateMousePosition : Float -> Origin -> MousePosition -> MousePosition
rotateMousePosition rotation ( originX, originY ) mousePos =
  let

    a = -rotation

    x = ( mousePos.clientX - originX )
    y = ( mousePos.clientY - originY )

    rx = ( x * cos a - y * sin a )
    ry = ( x * sin a + y * cos a )

    trx = rx + originX
    try = ry + originY

  in
  { mousePos | clientX = trx, clientY = try }





-- VIEW

view : Model -> Html Msg
view model =
  let

    lastTurnTime = Time.posixToMillis model.time // 1000 - model.lastTime

  in
  div
    -- Main container
    [ class "main"
    ]
    [ div
        -- Debugging messages
        [ class "debug-message"
        --, Html.Events.on "click" ( Json.Decode.map MirrorThePage decodeMousePosition )
        ]
        [ text model.text
        ]
    , div
        -- WebSocket connection closed message
        [ class "connection-lost"
        , style "display" ( if model.isWebSocketClosed then "flex" else "none" )
        ]
        [ text "Connection lost. Reload the page"
        ]
    , viewDraggables model
        -- draggablesView

    , div
        -- last turn info
        [ class "last-turn-info"
        , Html.Events.on "mouseenter" ( Json.Decode.succeed ( ShowLastDraggable ( List.head model.draggables ) ) )
        ]
        [ text ( "Last turn was done " ++ stringifyLastTurnTime lastTurnTime ++ " ago by " ++ stringifyLastOwner model.client model.lastOwner )
        ]

    , div
        -- settings
        [ class "settings"
        , style "display" ( if model.isSettingsShown then "flex" else "none" )
        ]
        [ input
            -- master volume
            [ Html.Attributes.type_ "range"
            , Html.Attributes.max "100"
            , Html.Attributes.min "0"
            , Html.Attributes.value ( String.fromFloat ( model.soundVolume * 100 ) )
            , Html.Events.on "change" ( Json.Decode.map SetSoundVolume decodeInputValue )
            ]
            [
            ]
        , div
            -- master volume label
            [
            ]
            [ text ( "Master Sound Volume : " ++ String.fromFloat model.soundVolume )
            ]
        , div
            -- screen rotation
            [ class "button-container"
            ]
            [ div
                [ class "button"
                , Html.Events.on "click" ( Json.Decode.succeed ( RotateScreen90deg Anticlockwise ) )
                ]
                [ text "↺"
                ]
            , div
                [ class "button"
                , Html.Events.on "click" ( Json.Decode.succeed ( RotateScreen90deg Clockwise ) )
                ]
                [ text "↻"
                ]
            , div
                [ class "button"
                , Html.Events.on "click" ( Json.Decode.succeed ( RotateScreen90deg Default ) )
                ]
                [ text "="
                ]
            ]
        , div
            [
            ]
            [ text ( "Screen rotation : " ++ radToDeg model.screenRotation )
            ]
        ]
    , div
        -- Settings
        [ class "settings-button button"
        , Html.Events.on "click" ( Json.Decode.map ShowSettings decodeMousePosition )
        ]
        [ text "⚙"
        ]
    ]



stringifyLastTurnTime : Int -> String
stringifyLastTurnTime time =
  String.join " " <| List.reverse <| List.map (\ ( y, x ) -> case x of
      0 -> ""
      _ -> String.fromInt x ++ y
    ) [ ( " seconds", modBy 60 time )
      , ( " minutes", modBy 60 ( time // 60 ) )
      , ( " hours", modBy 24 ( time // 60 // 60 ) )
      , ( " days", modBy 30 ( time // 60 // 60 // 24 ) )
      , ( " monthishes", modBy 12 ( time // 60 // 60 // 24 // 30 ) )
      , ( " yearishes", modBy 10 ( time // 60 // 60 // 24 // 30 // 12 ) )
      , ( " decadishes", modBy 10 ( time // 60 // 60 // 24 // 30 // 12 // 10 ) )
      , ( " centurishes", modBy 10 ( time // 60 // 60 // 24 // 30 // 12 // 10 // 10 ) )
      , ( " mileniumishes", modBy 418 ( time // 60 // 60 // 24 // 30 // 12 // 10 // 10 // 10 ) )
      , ( " Изге җомга көне мөбәрәк булсын", time // 60 // 60 // 24 // 30 // 12 // 10 // 10 // 10 // 418 )
      ]



stringifyLastOwner : Int -> Int -> String
stringifyLastOwner client owner =
  if client == owner then "you" else String.fromInt owner



type RotateDirection
  = Clockwise
  | Anticlockwise
  | Default


radToDeg : Float -> String
radToDeg rad =
  String.fromInt ( floor ( rad * 180 / pi ) ) ++ "deg"



decodeInputValue : Json.Decode.Decoder String
decodeInputValue =
  Json.Decode.at [ "target", "value" ] Json.Decode.string







type alias MousePosition =
  { clientX : Float
  , clientY : Float
  }

decodeMousePosition : Json.Decode.Decoder MousePosition
decodeMousePosition =
  Json.Decode.map2 MousePosition
    ( Json.Decode.at [ "clientX" ] Json.Decode.float )
    ( Json.Decode.at [ "clientY" ] Json.Decode.float )


decodeTouchPosition : Json.Decode.Decoder MousePosition
decodeTouchPosition =
  Json.Decode.map2 MousePosition
    ( Json.Decode.at [ "touches", "0", "clientX" ] Json.Decode.float )
    ( Json.Decode.at [ "touches", "0", "clientY" ] Json.Decode.float )
  --Json.Decode.at [ "touches" ] ( Json.Decode.list MousePosition )






viewDraggables : Model -> Html Msg
viewDraggables model =
  let
    draggables = model.draggables
    containerWidth = model.containerWidth
    containerHeight = model.containerHeight
    scalerX = model.scalerX
    scalerY = model.scalerY
    screenRotation = model.screenRotation

    styleScaler = styleScalerFor scalerX scalerY
    ( width, height ) = styleScaler containerWidth containerHeight
  in
  div
    [ class "draggables-container"
    , id "draggables-container"
    , style "width" width
    , style "height" height
    , style "transform" ( "rotate(" ++ radToDeg screenRotation ++ ")" )
    , Html.Events.on "mouseleave" ( Json.Decode.map MouseLeave decodeMousePosition )
    , Html.Events.on "mousemove" ( Json.Decode.map MouseMove decodeMousePosition )
    , Html.Events.on "mouseup" ( Json.Decode.map MouseUp decodeMousePosition )
    , Html.Events.on "touchcancel" ( Json.Decode.map MouseLeave decodeTouchPosition )
    , Html.Events.on "touchmove" ( Json.Decode.map MouseMove decodeTouchPosition )
    , Html.Events.on "touchend" ( Json.Decode.succeed ( TouchUp ) )
    ]
    ( viewDraggablesAsList draggables styleScaler model )






viewDraggablesAsList : List Draggable -> StyleScaler -> Model -> List ( Html Msg )
viewDraggablesAsList draggables styleScaler model =
  let
    isPageMirrored = model.isPageMirrored
    screenRotation = model.screenRotation
  in
  case draggables of
    [] -> []
    (draggable::tail) ->
      let

        ( width, height ) = styleScaler draggable.width draggable.height

        ( left, top ) = styleScaler ( draggable.x - draggable.offsetX ) ( draggable.y - draggable.offsetY )

        depth = draggable.depth + List.length tail -- Adding tail length reverses natural depth

        border = case model.lastDraggable of
          Nothing -> "initial"
          Just d -> if d == draggable then "2px dashed " ++ stringifyRGB ( hashRGB model.lastOwner ) else "initial"

      in
      div
        [ class "draggable"
        , style "transform" ( "rotate(" ++ radToDeg ( -screenRotation ) ++ ")" )
        , style "z-index" ( String.fromInt depth )
        , style "left" left
        , style "top" top
        , style "width" width
        , style "height" height
        , style "border" border
        , Html.Events.on "mousedown" ( Json.Decode.map ( DraggableMouseDown draggable ) decodeMousePosition )
        , Html.Events.on "touchstart" ( Json.Decode.map ( DraggableMouseDown draggable ) decodeTouchPosition )
        ]
        [ img [ src ( draggableTypeSrc draggable.type_ ) ] []
        ]
      :: viewDraggablesAsList tail styleScaler model















isDraggableActive : Draggable -> Maybe Draggable -> Bool
isDraggableActive draggable activeDraggable =
  case activeDraggable of
    Nothing -> False
    Just d -> draggable == d





type alias StyleScaler =
  Int -> Int -> ( String, String )


styleScalerFor : Float -> Float -> StyleScaler
styleScalerFor scalerA scalerB a b =
  ( String.fromFloat ( toFloat a * scalerA ) ++ "px", String.fromFloat ( toFloat b * scalerB ) ++ "px" )


getContainerElement : Cmd Msg
getContainerElement =
  Task.attempt ContainerElementGot ( Browser.Dom.getElement "draggables-container" )


draggableTypeSrc : Int -> String
draggableTypeSrc type_ =
  case type_ of
    0x01 -> "figures/white_queen.png"
    0x02 -> "figures/white_pawn.png"
    0x03 -> "figures/white_knight.png"
    0x04 -> "figures/white_rook.png"
    0x05 -> "figures/white_bishop.png"
    0x06 -> "figures/white_king.png"
    0x07 -> "figures/black_queen.png"
    0x08 -> "figures/black_pawn.png"
    0x09 -> "figures/black_knight.png"
    0x0a -> "figures/black_rook.png"
    0x0b -> "figures/black_bishop.png"
    0x0c -> "figures/black_king.png"
    0x0d -> "figures/white_coin.png"
    0x0e -> "figures/black_coin.png"
    _    -> "figures/unknown.png"





-- DRAGGABLE

type alias Draggable =
  { id      : Int
  , x       : Int
  , y       : Int
  , offsetX : Int
  , offsetY : Int
  , owner   : Int
  , type_   : Int
  , width   : Int
  , height  : Int
  , depth   : Int
  }


draggableFromRaw : List Int -> Maybe Draggable
draggableFromRaw list =
  case list of
    (id::x::y::offsetX::offsetY::owner::type_::depth::_) ->
      Just ( Draggable id x y offsetX offsetY owner type_ 64 64 depth )
    _ ->
      Nothing


draggableListFromRaw : List Int -> List Draggable
draggableListFromRaw list =
  case list of
    (id::x::y::offsetX::offsetY::owner::type_::depth::tail) ->
      Draggable id x y offsetX offsetY owner type_ 64 64 depth :: draggableListFromRaw tail
    _ -> []










-- HELPERS



stringifyMousePosition : MousePosition -> String
stringifyMousePosition mousePos =
  String.fromFloat mousePos.clientX ++ " " ++ String.fromFloat mousePos.clientY

stringifyListInt : List Int -> String
stringifyListInt list =
  case list of
    (x::xs) -> String.fromInt x ++ " " ++ stringifyListInt xs
    _       -> ""

stringifyWebSocket : IncomingWebSocket -> String
stringifyWebSocket msg =
  case msg of
    EmptyPortMessage      -> "EmptyPortMessage "
    WebSocketOpened       -> "WebSocketOpened "
    WebSocketClosed       -> "WebSocketClosed "
    Initialization   list -> "Initialization "
    DraggableGrabbed list -> "DraggableGrabbed " ++ stringifyListInt list
    DraggablePutted  list -> "DraggablePutted "  ++ stringifyListInt list
    DraggableMoved   list -> "DraggableMoved "   ++ stringifyListInt list
    IncorrectAction       -> "IncorrectAction "

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











