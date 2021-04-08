module RandomListOfTuple3 exposing ( main )

import Browser
import Html exposing (..)
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
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model ""
  , Random.generate ListOfTuple3Generated generateRandomListOfTuple3
  )

generateRandomInt : Random.Generator Int
generateRandomInt =
  Random.int 1 255

generateRandomTuple3 : Random.Generator Tuple3
generateRandomTuple3 =
  Random.map3 (\ a b c -> ( a, b, c )) generateRandomInt generateRandomInt generateRandomInt

generateRandomListOfTuple3 : Random.Generator ListOfTuple3
generateRandomListOfTuple3 =
  Random.list 2 generateRandomTuple3

-- UPDATE

type Msg
  = ListOfTuple3Generated ListOfTuple3

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ListOfTuple3Generated list ->
      ( { model | text = stringifyListOfTuple3 list }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [] [ text model.text ]

-- HELPERS

stringifyListOfTuple3 : ListOfTuple3 -> String
stringifyListOfTuple3 list =
  case list of
    ( x::xs ) -> stringifyTuple3 x ++ " | " ++ stringifyListOfTuple3 xs
    [] -> ""

stringifyTuple3 : Tuple3 -> String
stringifyTuple3 ( a, b, c ) =
  String.fromInt a ++ " " ++ String.fromInt b ++ " " ++ String.fromInt c
