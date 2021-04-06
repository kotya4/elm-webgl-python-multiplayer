-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

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


type alias Model =
  { text : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "fuck"
  , Random.generate ABC genL
  )


genL : Random.Generator L
genL = Random.Generator


--genL = Random.map3 (\a b c -> ( a, b, c )) ( Random.int ( 0 10 ) Random.int ( 0 10 ) Random.int ( 0 10 ) )

--map2 (\a b -> (a,b)) genA genB
-- UPDATE

type alias L = List ( Int, Int, Int )

type Msg
  = ABC L



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ABC list ->

      ( { model | text = printlist list }
      , Cmd.none
      )


printlist list =
  case list of
    ( x::xs ) -> printtuple x ++ " , " ++ printlist xs
    [] -> ""


printtuple tuple =
  case tuple of
    (a, b, c) -> String.fromInt a ++ " " ++ String.fromInt b ++ " " ++ String.fromInt c
    _ -> "fuck"

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  h1 [] [ text model.text ]
