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
import Array exposing ( Array )



port sendRequestPointerLock : () -> Cmd msg
port resvRequestPointerLock : ( Bool -> msg ) -> Sub msg





type Message
  = Nothing
  | NextFrame Float
  | KeyDown Int
  | KeyUp Int
  | SendRequestPointerLock
  | RotatePlayerCamera Int Int
  | UpdatePointerLock Bool



type alias Keyboard =
  Array Bool
defaultKeyboard : Keyboard
defaultKeyboard = Array.repeat 256 False





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
  , looksAt : Math.Vector3.Vec3
  , pitch : Float
  , yaw : Float
  }
defaultPlayer : Player
defaultPlayer = Player 0 0 -15 Math.Vector3.k 0 0


type alias Model =
  { time : Float
  , mouse : Mouse
  , player : Player
  , keyboard : Keyboard
  }
defaultModel : Model
defaultModel = Model 0 defaultMouse defaultPlayer defaultKeyboard




main : Program () Model Message
main =
  let
    keydown = Json.Decode.map KeyDown ( Json.Decode.field "keyCode" Json.Decode.int )
    keyup = Json.Decode.map KeyUp ( Json.Decode.field "keyCode" Json.Decode.int )
    click = Json.Decode.succeed SendRequestPointerLock
    noclick = Json.Decode.succeed Nothing
    mousemove = Json.Decode.map2 RotatePlayerCamera
      ( Json.Decode.field "movementX" Json.Decode.int )
      ( Json.Decode.field "movementY" Json.Decode.int )
    nomousemove = Json.Decode.succeed Nothing
  in
  Browser.document
    { init = \ _ -> ( defaultModel, Cmd.none )
    , view = \ model -> viewDocument "webgl" model
    , update = update
    , subscriptions = \ model -> Sub.batch
        [ Browser.Events.onAnimationFrameDelta NextFrame
        , Browser.Events.onKeyDown keydown
        , Browser.Events.onKeyUp keyup
        , Browser.Events.onClick ( if model.mouse.locked then noclick else click )
        , Browser.Events.onMouseMove ( if model.mouse.locked then mousemove else nomousemove )
        , resvRequestPointerLock UpdatePointerLock
        ]
    }



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




update : Message -> Model -> ( Model, Cmd msg )
update message model =
  case message of

    Nothing ->
      ( model , Cmd.none )



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
        ( fx, fy, fz ) = tuplify player.looksAt
        ( rx, ry, rz ) = rotateVec3 ( pi / 2 ) Y player.looksAt |> tuplify
        ( ux, uy, uz ) = tuplify Math.Vector3.j
        w = Array.get 87 model.keyboard |> justkey -- w
        s = Array.get 83 model.keyboard |> justkey -- s
        a = Array.get 65 model.keyboard |> justkey -- a
        d = Array.get 68 model.keyboard |> justkey -- d
        q = Array.get 16 model.keyboard |> justkey -- shift
        e = Array.get 17 model.keyboard |> justkey -- ctrl
        ( fdx, fdy, fdz ) = if w then ( fx, fy, fz ) else if s then ( -fx, -fy, -fz ) else ( 0, 0, 0 )
        ( rdx, rdy, rdz ) = if a then ( rx, ry, rz ) else if d then ( -rx, -ry, -rz ) else ( 0, 0, 0 )
        ( udx, udy, udz ) = if q then ( ux, uy, uz ) else if e then ( -ux, -uy, -uz ) else ( 0, 0, 0 )
        x = player.x + fdx + rdx + udx
        y = player.y + fdy + rdy + udy
        z = player.z + fdz + rdz + udz
        newplayer = { player | x = x, y = y, z = z }
      in
      ( { model | time = newtime, player = newplayer } , Cmd.none )



    KeyDown key ->
      let
        newmodel = { model | keyboard = Array.set key True model.keyboard }
        --d = Debug.log "key" key
      in
      case key of
        _ -> ( newmodel , Cmd.none )



    KeyUp key ->
      let
        newmodel = { model | keyboard = Array.set key False model.keyboard }
      in
      case key of
        _ -> ( newmodel , Cmd.none )



    SendRequestPointerLock ->
      let
        d = Debug.log "request" "sent"
      in
      ( model, sendRequestPointerLock () )



    RotatePlayerCamera dx dy ->
      let
        mouse = model.mouse
        newmouse = { mouse | movementX = dx, movementY = dy }
        player = model.player
        mousespeed = 0.01
        pitch = player.pitch + toFloat dy * mousespeed |> min ( pi / 2 ) |> max ( -pi / 2 )
        yaw = player.yaw - toFloat dx * mousespeed
        looksat = rotateVec3 yaw Y ( rotateVec3 pitch X Math.Vector3.k )
        newplayer = { player | looksAt = looksat, pitch = pitch, yaw = yaw }

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




bodyAttributes : List ( Html.Attribute Message )
bodyAttributes =
  [ Html.Attributes.style "position" "absolute"
  , Html.Attributes.style "left" "0"
  , Html.Attributes.style "top" "0"
  , Html.Attributes.style "width" "100vw"
  , Html.Attributes.style "height" "100vh"
  , Html.Attributes.style "background" "#1c1c1a"
  , Html.Attributes.style "display" "flex"
  , Html.Attributes.style "justify-content" "center"
  , Html.Attributes.style "align-item" "center"
  ]



viewDocument : String -> Model -> Browser.Document Message
viewDocument title model =
  { title = title
  , body =
      [ div
          ( bodyAttributes
          )
          [ view model
          ]
      ]
  }







webglcontextAttributes : Int -> Int -> List ( Html.Attribute msg )
webglcontextAttributes width height =
  [ Html.Attributes.width width
  , Html.Attributes.height height
  , Html.Attributes.style "image-rendering" "pixelated"
  ]



view : Model -> Html Message
view { time, player } =
  let
    width = 400
    height = 400

    eye = Math.Vector3.vec3 player.x player.y player.z
    center = Math.Vector3.add eye player.looksAt
    up = Math.Vector3.j

    perspective =
      Math.Matrix4.mul
        ( Math.Matrix4.makePerspective 45 ( width / height ) 0.01 100 )
        ( Math.Matrix4.makeLookAt eye center up )
    notime = 0xbabea7e70907a7
    rotation =
      Math.Matrix4.mul
        ( Math.Matrix4.makeRotate ( 3 * notime / 1000 ) Math.Vector3.j )
        ( Math.Matrix4.makeRotate ( 2 * notime / 1000 ) Math.Vector3.i )

  in
  WebGL.toHtml
    ( webglcontextAttributes width height
    )
    [ WebGL.entity
        vertexShader
        fragmentShader
        ( mesh
            [ cubeSide PositiveX
            , cubeSide PositiveY
            , cubeSide PositiveZ
            , cubeSide NegativeX
            , cubeSide NegativeY
            , cubeSide NegativeZ
            ] )
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




