module Slugify where

import Debug
import Graphics.Element as Element
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Regex
import Signal (..)
import String
import Window

-- Model

type alias State =
  { text : String }

initialState : State
initialState =
  { text = "" }

type Update
  = NoOp
  | Change String

-- Input

updates : Channel Update
updates = channel NoOp

-- Update

main : Signal Element.Element
main = render <~ state
               ~ Window.dimensions

state : Signal State
state = foldp step initialState (subscribe updates)

step : Update -> State -> State
step update state =
  let
    _ = Debug.watch "update" update
  in
    case update of
      Change str -> { state | text <- str }
      _ -> state

-- View

render : State -> (Int,Int) -> Element.Element
render state (w,h) =
  let
    _ = Debug.watch "cleaned up" (cleanup state.text)
  in
    toElement w h <|
      div
        [ class "test"
        , containerStyle
        ]
        [ node "h3" [] [ text "Slugify.elm" ]
        , spacer "1em"
        , input
            [ inputStyle
            , on "keyup" targetValue (\val -> send updates (Change val))
            , value state.text
            ]
            []
        , spacer "1em"
        , div [ outputStyle ] [ text <| cleanup state.text ]
        ]

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
    [ ("margin", "0 1em")
    , ("width", "50%")
    , ("max-width", "30em")
    , ("font-size", "2em")
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
