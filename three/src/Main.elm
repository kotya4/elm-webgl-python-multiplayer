module Main exposing ( main )
import Html exposing ( Html, div, text )
import Html.Attributes
import Browser
import Browser.Events
import WebGL
import Math.Vector3
import Math.Matrix4







type Message =
  NextFrame Float


type alias Model =
  Float


main : Program () Model Message
main =
  Browser.document
    { init = \ _ -> ( 0, Cmd.none )
    , view = \ model -> viewDocument "webgl" model
    , update = update
    , subscriptions = \ _ -> Browser.Events.onAnimationFrameDelta NextFrame
    }


update : Message -> Model -> ( Model, Cmd Message )
update msg time =
  case msg of
    NextFrame delta ->
      ( time + delta, Cmd.none )



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







view : Model -> Html Message
view time =
  let
    width = 400
    height = 400
    eye = Math.Vector3.vec3 0 0 -5
    center = Math.Vector3.vec3 0 0 1
    up = Math.Vector3.vec3 0 1 0
    perspective =
      Math.Matrix4.mul
        ( Math.Matrix4.makePerspective 45 ( width / height ) 0.01 100 )
        ( Math.Matrix4.makeLookAt eye center up )
    notime = time -- 0xbabea7e70907a7
    rotation =
      Math.Matrix4.mul
        ( Math.Matrix4.makeRotate ( 3 * notime / 1000 ) ( Math.Vector3.vec3 0 1 0 ) )
        ( Math.Matrix4.makeRotate ( 2 * notime / 1000 ) ( Math.Vector3.vec3 1 0 0 ) )
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




webglcontextAttributes : Int -> Int -> List ( Html.Attribute msg )
webglcontextAttributes width height =
  [ Html.Attributes.width width
  , Html.Attributes.height height
  , Html.Attributes.style "image-rendering" "pixelated"
  ]


bodyAttributes : List ( Html.Attribute msg )
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
