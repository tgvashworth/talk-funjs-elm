module Username where

import Debug
import Graphics.Element as Element
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Regex
import Signal (..)
import String
import Window

type alias State =
  { username : String }

type Update
  = NoOp
  | Change String

main : Signal Element.Element
main = render <~ state
               ~ Window.dimensions

state : Signal State
state = foldp step initialState (subscribe updates)

render : State -> (Int,Int) -> Element.Element
render state (w,h) =
  let
    _ = Debug.watch "state.username" state.username
    _ = Debug.watch "cleanup state.username" (cleanup state.username)
  in
    toElement w h <|
      div
        [ class "test"
        , containerStyle
        ]
        [ spacer "1em"
        , input
            [ inputStyle
            , on "keyup" targetValue (\val -> send updates (Change val))
            , value state.username
            ]
            []
        , spacer "1em"
        , div [ outputStyle ] [ text <| cleanup state.username ]
        ]

step : Update -> State -> State
step update state =
  let
    _ = Debug.watch "update" update
  in
    case update of
      Change str -> { state | username <- str }
      _ -> state

initialState : State
initialState =
  { username = "" }

updates : Channel Update
updates = channel NoOp

-- Logic

cleanup : String -> String
cleanup =
  String.trim >>
  Regex.replace Regex.All punctuation (always "") >>
  Regex.replace Regex.All multispace (always " ") >>
  String.split " " >>
  String.join "-"

-- Utils

spacer : String -> Html
spacer h =
  div [ style [ ("height", h) ] ] []

punctuation : Regex.Regex
punctuation = Regex.caseInsensitive (Regex.regex "[^a-z -]")

multispace = Regex.regex " {1,}"

-- Styles

containerStyle : Attribute
containerStyle =
  style
    [ ("margin", "0 auto")
    , ("width", "95%")
    , ("max-width", "30em")
    ]

inputStyle : Attribute
inputStyle =
  style
    [ ("font-size", "2em")
    , ("width", "100%")
    ]

outputStyle : Attribute
outputStyle =
  style
    [ ("font-size", "2em")
    ]
