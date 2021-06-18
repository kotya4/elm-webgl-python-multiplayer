port module Main exposing ( main )
import Html exposing ( Html, div, text )
import Html.Attributes
import Html.Events
import Browser
import Browser.Events
import WebGL
import Math.Vector3
import Math.Matrix4
import Json.Decode
import Json.Encode
import Array exposing ( Array )
import Time



port sendRequestPointerLock : () -> Cmd msg
port resvRequestPointerLock : ( Bool -> msg ) -> Sub msg
port sendLocalStorage : String -> Cmd msg





type Message
  = ElmoElskerSoccer
  | NextFrame Float
  | KeyDown Int
  | KeyUp Int
  | SendRequestPointerLock
  | RotatePlayerCamera Int Int
  | UpdatePointerLock Bool
  | UpdateLocalStorage Time.Posix
  | ResizeDisplay Int Int






type alias Mouse =
  { locked : Bool
  , movementX : Int
  , movementY : Int
  }
defaultMouse : Mouse
defaultMouse = Mouse False 0 0



type alias Player =
  { x : Float
  , y : Float
  , z : Float
  , pitch : Float
  , yaw : Float
  }
defaultPlayer : Player
defaultPlayer = Player 0 0 -15 0 0


type alias Keyboard =
  Array Bool
defaultKeyboard : Keyboard
defaultKeyboard = Array.repeat 256 False






type alias Model =
  { time : Float
  , mouse : Mouse
  , player : Player
  , keyboard : Keyboard
  , storageTimer : Float
  , size : ( Float, Float )
  }
defaultModel : Model
defaultModel = Model 0 defaultMouse defaultPlayer defaultKeyboard 0 ( 0, 0 )




main : Program Json.Decode.Value Model Message
main =
  let
    keydown = Json.Decode.map KeyDown ( Json.Decode.field "keyCode" Json.Decode.int )
    keyup = Json.Decode.map KeyUp ( Json.Decode.field "keyCode" Json.Decode.int )
  in
  Browser.document
    { init = init
    , view = \ model -> viewDocument "webgl" model
    , update = update
    , subscriptions = \ model -> Sub.batch
        [ Browser.Events.onAnimationFrameDelta NextFrame
        , Browser.Events.onKeyDown keydown
        , Browser.Events.onKeyUp keyup
        , Browser.Events.onResize ResizeDisplay
        , resvRequestPointerLock UpdatePointerLock
        ]
    }




init : Json.Decode.Value -> ( Model, Cmd msg )
init f =
  let

    size =
      Json.Decode.map2 ( \ a b -> ( a, b ) )
        ( Json.Decode.at [ "0" ] Json.Decode.int )
        ( Json.Decode.at [ "1" ] Json.Decode.int )

    stor = Json.Decode.map5 Player
          ( Json.Decode.at [ "x" ] Json.Decode.float )
          ( Json.Decode.at [ "y" ] Json.Decode.float )
          ( Json.Decode.at [ "z" ] Json.Decode.float )
          ( Json.Decode.at [ "pitch" ] Json.Decode.float )
          ( Json.Decode.at [ "yaw" ] Json.Decode.float )


    flags =
      Json.Decode.map2 ( \ st si -> ( st, si ) )
        ( Json.Decode.at [ "storage" ] ( Json.Decode.nullable stor ) )
        ( Json.Decode.at [ "size" ] size )


    ( maybestorage, ( w, h ) ) =
      case Json.Decode.decodeValue flags f of
        Ok o -> o
        Err _ -> ( Just defaultPlayer, ( -1, 0 ) )


    storage =
      case maybestorage of
        Just v -> v
        Nothing -> { defaultPlayer | x = -1 }


    d = Debug.log "init" ( storage, ( w, h ) )

  in
    ( { defaultModel | player = storage, size = ( toFloat w, toFloat h ) } , Cmd.none )










type Axis3
  = X
  | Y
  | Z


rotateVec3 : Float -> Axis3 -> Math.Vector3.Vec3 -> Math.Vector3.Vec3
rotateVec3 angle axis v =
  let
    x = Math.Vector3.getX v
    y = Math.Vector3.getY v
    z = Math.Vector3.getZ v
    c = cos angle
    s = sin angle
  in
    case axis of
      X -> Math.Vector3.vec3 ( x ) ( y * c - z * s ) ( y * s + z * c )
      Y -> Math.Vector3.vec3 ( x * c + z * s ) ( y ) ( z * c - x * s )
      Z -> Math.Vector3.vec3 ( x * c - y * s ) ( x * s + y * c ) ( z )


forwardVec3 : Float -> Float -> Math.Vector3.Vec3
forwardVec3 pitch yaw =
  rotateVec3 yaw Y ( rotateVec3 pitch X Math.Vector3.k )



keycodes : List ( String, Int )
keycodes =
  [ ( "w",     87 )
  , ( "s",     83 )
  , ( "a",     65 )
  , ( "d",     68 )
  , ( "space", 32 )
  , ( "shift", 16 )
  , ( "1",     49 )
  , ( "←",     37 )
  , ( "↑",     38 )
  , ( "→",     39 )
  , ( "↓",     40 )
  ]



findCodeByKey : String -> Int
findCodeByKey k =
  let
    find l =
      case l of
        ((key,code)::xs) -> if key == k then code else find xs
        [] -> -1
  in
  find keycodes




update : Message -> Model -> ( Model, Cmd msg )
update message model =
  case message of

    ResizeDisplay w h ->
      ( { model | size = ( toFloat w, toFloat h ) } , Cmd.none )


    NextFrame delta ->
      let
        newtime = model.time + delta
        player = model.player
        justkey k =
          case k of
            Just v -> v
            _ -> False
        movespeed = 0.1
        tuplify v =
          ( ( Math.Vector3.getX v ) * movespeed
          , ( Math.Vector3.getY v ) * movespeed
          , ( Math.Vector3.getZ v ) * movespeed
          )
        f = forwardVec3 player.pitch player.yaw
        ( fx, fy, fz ) = tuplify f
        ( rx, ry, rz ) = rotateVec3 ( pi / 2 ) Y f |> tuplify
        ( ux, uy, uz ) = tuplify Math.Vector3.j
        w = Array.get ( findCodeByKey "w"     ) model.keyboard |> justkey
        s = Array.get ( findCodeByKey "s"     ) model.keyboard |> justkey
        a = Array.get ( findCodeByKey "a"     ) model.keyboard |> justkey
        d = Array.get ( findCodeByKey "d"     ) model.keyboard |> justkey
        q = Array.get ( findCodeByKey "space" ) model.keyboard |> justkey
        e = Array.get ( findCodeByKey "shift" ) model.keyboard |> justkey
        ( fdx, fdy, fdz ) = if w then ( fx, fy, fz ) else if s then ( -fx, -fy, -fz ) else ( 0, 0, 0 )
        ( rdx, rdy, rdz ) = if a then ( rx, ry, rz ) else if d then ( -rx, -ry, -rz ) else ( 0, 0, 0 )
        ( udx, udy, udz ) = if q then ( ux, uy, uz ) else if e then ( -ux, -uy, -uz ) else ( 0, 0, 0 )
        x = player.x + fdx + rdx + udx
        y = player.y + fdy + rdy + udy
        z = player.z + fdz + rdz + udz
        newplayer = { player | x = x, y = y, z = z }
        newstoragetimer = model.storageTimer + delta
        newmodel = { model | time = newtime, player = newplayer, storageTimer = newstoragetimer }



        al = Array.get ( findCodeByKey "←"     ) model.keyboard |> justkey
        at = Array.get ( findCodeByKey "↑"     ) model.keyboard |> justkey
        ar = Array.get ( findCodeByKey "→"     ) model.keyboard |> justkey
        ab = Array.get ( findCodeByKey "↓"     ) model.keyboard |> justkey
        dx = if al then -1 else if ar then 1 else 0
        dy = if at then -1 else if ab then 1 else 0
        rotatedisplay (m,c) = if dx /= 0 || dy /= 0 then update ( RotatePlayerCamera dx dy ) m else (m,c)
        --rotatedisplay m = update ( RotatePlayerCamera dx dy ) m

      in
      rotatedisplay
        ( if newstoragetimer > 10000 then -- every 10 sec.
            let
              d2 = Debug.log "storagetimer" "ok"
              mewstorage = Json.Encode.encode 0
                ( Json.Encode.object
                    [ ( "x", Json.Encode.float player.x )
                    , ( "y", Json.Encode.float player.y )
                    , ( "z", Json.Encode.float player.z )
                    , ( "pitch", Json.Encode.float player.pitch )
                    , ( "yaw", Json.Encode.float player.yaw )
                    ]
                )
            in
            ( { newmodel | storageTimer = 0 }
            , sendLocalStorage mewstorage
            )
          else
            ( newmodel
            , Cmd.none
            )
        )



    KeyDown code ->
      let
        newmodel = { model | keyboard = Array.set code True model.keyboard }
        d = Debug.log "key" code
      in
      if code == findCodeByKey "1" then
        let
          player = newmodel.player
        in
        update
          ( RotatePlayerCamera 0 0 )
          { newmodel | player = { player | yaw = 0, pitch = 0, x = 0, y = 0, z = -15 } }
      else
        ( newmodel , Cmd.none )



    KeyUp code ->
      let
        newmodel = { model | keyboard = Array.set code False model.keyboard }
      in
      case code of
        _ -> ( newmodel , Cmd.none )



    SendRequestPointerLock ->
      let
        d = Debug.log "request" "sent"
      in
      ( model, sendRequestPointerLock () )



    RotatePlayerCamera dx dy ->
      -- known bug: mousemove fires couple times on pointer
      -- lock turning on/off when console is open.
      let
        mouse = model.mouse
        newmouse = { mouse | movementX = dx, movementY = dy }
        player = model.player
        mousespeed = 0.01
        pitch = player.pitch + toFloat dy * mousespeed |> min ( pi / 2 ) |> max ( -pi / 2 )
        yaw = player.yaw - toFloat dx * mousespeed
        newplayer = { player | pitch = pitch, yaw = yaw }

        --d = Debug.log "move" mouse
      in
      ( { model | mouse = newmouse, player = newplayer } , Cmd.none )



    UpdatePointerLock locked ->
      let
        d = Debug.log "locked" locked
        mouse = model.mouse
        newmouse = { mouse | locked = locked, movementX = 0, movementY = 0 }
      in
      ( { model | mouse = newmouse }, Cmd.none )


    _ ->
      ( model , Cmd.none )






viewDocument : String -> Model -> Browser.Document Message
viewDocument title model =
  { title = title
  , body =
      [ div
          [ Html.Attributes.style "position" "absolute"
          , Html.Attributes.style "left" "0"
          , Html.Attributes.style "top" "0"
          , Html.Attributes.style "width" "100vw"
          , Html.Attributes.style "height" "100vh"
          , Html.Attributes.style "background" "#1c1c1a"
          , Html.Attributes.style "background-image" "url('chort.png')"
          , Html.Attributes.style "display" "flex"
          , Html.Attributes.style "justify-content" "center"
          , Html.Attributes.style "align-item" "center"
          ]
          [ view model
          , viewKeyButtons model
          ]
      ]
  }




viewKeyButtons : Model -> Html Message
viewKeyButtons { size } =
  let
    ( w, h ) = size
    m = ( min w h ) / 100

    font    = String.fromFloat ( m * 2.0 ) ++ "px"
    height  = String.fromFloat ( m * 2.5 ) ++ "px"
    margin  = String.fromFloat ( m * 1.0 ) ++ "px"
    border  = String.fromFloat ( m * 2.0 ) ++ "px"
    padding = String.fromFloat ( m * 2.0 ) ++ "px"


    button ( key, code ) =
      div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "padding" ( padding ++ " " ++ padding )
        , Html.Attributes.style "background" "grey"
        , Html.Attributes.style "color" "black"
        , Html.Attributes.style "border-radius" border
        , Html.Attributes.style "margin" margin
        , Html.Attributes.style "height" height
        , Html.Attributes.style "font-size" font
        , Html.Attributes.style "user-select" "none"
        , Html.Events.on "mousedown" ( Json.Decode.succeed ( KeyDown code ) )
        , Html.Events.on "mouseup" ( Json.Decode.succeed ( KeyUp code ) )
        ]
        [ text key
        ]

    foreach l =
      case l of
        (keycode::xs) -> button keycode :: foreach xs
        [] -> []
  in
  div
    [ Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "left" "0"
    , Html.Attributes.style "top" "0"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "justify-content" "center"
    , Html.Attributes.style "align-item" "center"
    ]
    ( foreach keycodes )





view : Model -> Html Message
view { time, player, size, mouse } =
  let
    ( w, h ) = size

    maxsize = 400.0

    ( width, height ) =
      if w < h then
        ( maxsize * w / h, maxsize )
      else
        ( maxsize, maxsize * h / w )

    eye = Math.Vector3.vec3 player.x player.y player.z
    center = Math.Vector3.add eye ( forwardVec3 player.pitch player.yaw )
    up = Math.Vector3.j

    perspective =
      Math.Matrix4.mul
        ( Math.Matrix4.makePerspective 70 ( width / height ) 0.01 100 )
        ( Math.Matrix4.makeLookAt eye center up )
    elmovilværedin = 0xbabea7e70907a7
    rotation =
      Math.Matrix4.mul
        ( Math.Matrix4.makeRotate ( 0.5 * time / 1000 ) Math.Vector3.j )
        ( Math.Matrix4.makeRotate ( 0.2 * time / 1000 ) Math.Vector3.i )

    mousedown =
      if mouse.locked then
        ( Json.Decode.succeed ElmoElskerSoccer )
      else
        ( Json.Decode.succeed SendRequestPointerLock )

    mousemove =
      if mouse.locked then
        ( Json.Decode.map2 RotatePlayerCamera
          ( Json.Decode.field "movementX" Json.Decode.int )
          ( Json.Decode.field "movementY" Json.Decode.int )
        )
      else
        ( Json.Decode.succeed ElmoElskerSoccer )

  in
  WebGL.toHtml
    [ Html.Attributes.width ( floor width )
    , Html.Attributes.height ( floor height )
    , Html.Attributes.style "image-rendering" "pixelated"
    , Html.Events.on "mousedown" mousedown
    , Html.Events.on "mousemove" mousemove
    ]
    [ WebGL.entity
        vertexShader
        fragmentShader
        ( mesh ( world player ) )
        ( Uniforms time perspective rotation )
    ]


type alias Uniforms =
  { time : Float
  , perspective : Math.Matrix4.Mat4
  , rotation : Math.Matrix4.Mat4
  }


type alias Attributes =
  { position : Math.Vector3.Vec3
  , color : Math.Vector3.Vec3
  }


type alias Triangles =
  List ( Attributes, Attributes, Attributes )


type CubeSide
  = PositiveZ
  | NegativeZ
  | PositiveY
  | NegativeY
  | PositiveX
  | NegativeX


mesh : List Triangles -> WebGL.Mesh Attributes
mesh meshes =
  let
    flat a =
      case a of
        (t::ts) -> t ++ flat ts
        [] -> []
  in
    flat meshes |> WebGL.triangles




world : Player -> List Triangles
world { x, y, z } =
  let
    size = 5

    chunk = List.repeat ( size * size * size ) 1

    indexed = List.indexedMap Tuple.pair chunk

    cubesize = 2 -- size produced by "cube"

    w = size
    h = size
    d = size
    xyz i = Math.Vector3.vec3 -- reverse of " ( w * h * z ) + ( w * y ) + x == i "
      ( ( i           |> modBy w |> toFloat ) * cubesize )
      ( ( i // w      |> modBy h |> toFloat ) * cubesize )
      ( ( i // w // h |> modBy d |> toFloat ) * cubesize )

    void = []

    fill ivs =
      case ivs of
        ((i,v)::s) -> translate ( xyz i ) ( if modBy 3 i > 1 then cube else void ) :: fill s
        [] -> []

    flat a =
      case a of
        (t::ts) -> t ++ flat ts
        [] -> []

  in
    fill indexed |> flat







translate : Math.Vector3.Vec3 -> List Triangles -> List Triangles
translate v tris =
  let
    t { position, color } = Attributes ( Math.Vector3.add position v ) color
  in
    List.map ( \ list -> List.map ( \ ( a, b, c ) -> ( t a, t b, t c ) ) list ) tris





cube : List Triangles
cube =
  [ cubeSide PositiveX
  , cubeSide PositiveY
  , cubeSide PositiveZ
  , cubeSide NegativeX
  , cubeSide NegativeY
  , cubeSide NegativeZ
  ]




cubeSide : CubeSide -> Triangles
cubeSide side =
  let
    vec3 x y z = Math.Vector3.vec3 x y z
    attr position color = Attributes position color
    face a b c = ( a, b, c )
    col = vec3
  in
  case side of
    PositiveX ->
      [ face ( attr ( vec3  1  1  1 ) ( col 1 1 1 ) )
             ( attr ( vec3  1  1 -1 ) ( col 0 1 1 ) )
             ( attr ( vec3  1 -1 -1 ) ( col 0 0 1 ) )
      , face ( attr ( vec3  1 -1 -1 ) ( col 0 1 0 ) )
             ( attr ( vec3  1 -1  1 ) ( col 1 1 0 ) )
             ( attr ( vec3  1  1  1 ) ( col 1 0 1 ) )
      ]
    NegativeX ->
      [ face ( attr ( vec3 -1  1  1 ) ( col 1 1 1 ) )
             ( attr ( vec3 -1  1 -1 ) ( col 0 1 1 ) )
             ( attr ( vec3 -1 -1 -1 ) ( col 0 0 1 ) )
      , face ( attr ( vec3 -1 -1 -1 ) ( col 0 1 0 ) )
             ( attr ( vec3 -1 -1  1 ) ( col 1 1 0 ) )
             ( attr ( vec3 -1  1  1 ) ( col 1 0 1 ) )
      ]
    PositiveY ->
      [ face ( attr ( vec3  1  1  1 ) ( col 1 1 1 ) )
             ( attr ( vec3  1  1 -1 ) ( col 0 1 1 ) )
             ( attr ( vec3 -1  1 -1 ) ( col 0 0 1 ) )
      , face ( attr ( vec3 -1  1 -1 ) ( col 0 1 0 ) )
             ( attr ( vec3 -1  1  1 ) ( col 1 1 0 ) )
             ( attr ( vec3  1  1  1 ) ( col 1 0 1 ) )
      ]
    NegativeY ->
      [ face ( attr ( vec3  1 -1  1 ) ( col 1 1 1 ) )
             ( attr ( vec3  1 -1 -1 ) ( col 0 1 1 ) )
             ( attr ( vec3 -1 -1 -1 ) ( col 0 0 1 ) )
      , face ( attr ( vec3 -1 -1 -1 ) ( col 0 1 0 ) )
             ( attr ( vec3 -1 -1  1 ) ( col 1 1 0 ) )
             ( attr ( vec3  1 -1  1 ) ( col 1 0 1 ) )
      ]
    NegativeZ ->
      [ face ( attr ( vec3 -1 -1 -1 ) ( col 1 1 1 ) )
             ( attr ( vec3 -1  1 -1 ) ( col 0 1 1 ) )
             ( attr ( vec3  1  1 -1 ) ( col 0 0 1 ) )
      , face ( attr ( vec3  1  1 -1 ) ( col 0 1 0 ) )
             ( attr ( vec3  1 -1 -1 ) ( col 1 1 0 ) )
             ( attr ( vec3 -1 -1 -1 ) ( col 1 0 1 ) )
      ]
    PositiveZ ->
      [ face ( attr ( vec3  1  1  1 ) ( col 1 1 1 ) )
             ( attr ( vec3  1 -1  1 ) ( col 0 1 1 ) )
             ( attr ( vec3 -1 -1  1 ) ( col 0 0 1 ) )
      , face ( attr ( vec3 -1 -1  1 ) ( col 0 1 0 ) )
             ( attr ( vec3 -1  1  1 ) ( col 1 1 0 ) )
             ( attr ( vec3  1  1  1 ) ( col 1 0 1 ) )
      ]




vertexShader : WebGL.Shader Attributes Uniforms { vcolor : Math.Vector3.Vec3 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;
    uniform mat4 perspective;
    uniform mat4 rotation;
    varying vec3 vcolor;

    void main () {
      gl_Position = perspective * rotation * vec4 ( position, 1.0 );
      vcolor = color;
    }
  |]




fragmentShader : WebGL.Shader {} Uniforms { vcolor : Math.Vector3.Vec3 }
fragmentShader =
  [glsl|
    precision mediump float;
    uniform float time;
    varying vec3 vcolor;

    void main () {
      gl_FragColor = vec4 ( 1.0 / mod ( ( time / 500.0 + 1.0 ), 10.0 ), vcolor.y, vcolor.z, 1.0 );
    }
  |]




