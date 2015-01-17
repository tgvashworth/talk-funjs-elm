module Username where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Signal (..)
import String (..)
import Graphics.Element as Element
import Window

type alias State =
  { username : String }

type Update = NoOp | Change String

main : Signal Element.Element
main = render <~ state
               ~ Window.dimensions

state : Signal State
state = foldp step initialState (subscribe updates)

render : State -> (Int,Int) -> Element.Element
render state (w,h) =
  toElement w h <|
    div
      [ class "test"
      , containerStyle
      ]
      [ spacer "1em"
      , input
          [ inputStyle
          , on "keyup" targetValue (\val -> send updates (Change val))
          ]
          []
      , spacer "1em"
      , div [ outputStyle ] [ text <| cleanup state.username ]
      ]

step : Update -> State -> State
step update state =
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
cleanup = trim >> split " " >> join "-"

-- Utils

spacer : String -> Html
spacer h =
  div [ style [ ("height", h) ] ] []

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
