module Username where

import Html (..)
import Signal (..)
import Graphics.Element as Element
import Window

type alias State = {}
type Update = NoOp

main : Signal Element.Element
main = render <~ state
               ~ Window.dimensions

state : Signal State
state = foldp step initialState (subscribe updates)

render : State -> (Int,Int) -> Element.Element
render state (w,h) = toElement w h (text "Hello, world.")

step : Update -> State -> State
step _ state = state

initialState : State
initialState = {}

updates : Channel Update
updates = channel NoOp
