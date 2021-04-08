module HashedTuple3OnClick exposing ( main )

import Browser
import Html exposing (..)
import Html.Events exposing ( onClick, onInput )
import Random

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Tuple3 = ( Int, Int, Int )

type alias ListOfTuple3 = List Tuple3

type alias Model =
  { text : String
  , inputValue : String
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" ""
  , Cmd.none
  )

generateRandomInt : Random.Generator Int
generateRandomInt =
  Random.int 1 255

generateRandomTuple3 : Random.Generator Tuple3
generateRandomTuple3 =
  Random.map3 (\ a b c -> ( a, b, c )) generateRandomInt generateRandomInt generateRandomInt

color : Int -> Tuple3
color seed =
  let
    seed0 = Random.initialSeed seed
  in
    case Random.step generateRandomTuple3 seed0 of
      ( value, _ ) -> value

-- UPDATE

type Msg
  = OnInput String
  | OnClick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnInput value ->
      ( { model | inputValue = value }
      , Cmd.none
      )

    OnClick ->
      let
        maybeSeed = String.toInt model.inputValue
        seed = Maybe.withDefault 0 maybeSeed
      in
      ( { model | text = stringifyTuple3 ( color seed ) }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div
    []
    [ button [ onClick OnClick ] [ text "Click me" ]
    , input [ onInput OnInput ] []
    , div [] [ text model.text ]
    ]

-- HELPERS

stringifyListOfTuple3 : ListOfTuple3 -> String
stringifyListOfTuple3 list =
  case list of
    ( x::xs ) -> stringifyTuple3 x ++ " | " ++ stringifyListOfTuple3 xs
    [] -> ""

stringifyTuple3 : Tuple3 -> String
stringifyTuple3 ( a, b, c ) =
  String.fromInt a ++ " " ++ String.fromInt b ++ " " ++ String.fromInt c
