module Presentation where

import Debug
import Dict
import Graphics.Element
import Html (..)
import Html.Events (..)
import Html.Attributes (..)
import Keyboard
import List
import Markdown
import Signal
import Signal (..)
import Window

-- Model

type alias AppState =
  { currentSlide : Int
  }

initialAppState : AppState
initialAppState =
  { currentSlide = 0
  }

type Input =
  NoOp
  | ChangeSlide Int

type alias Slide = (String, Html)

type alias Style = Dict.Dict String String

-- Main

main : Signal Graphics.Element.Element
main = renderMain <~ appStateSignal ~ Window.dimensions

appStateSignal : Signal AppState
appStateSignal = Signal.foldp step initialAppState inputSignal

step : Input -> AppState -> AppState
step input state =
  let
    _ = Debug.watch "input" input
  in
    case input of
      ChangeSlide i -> { state | currentSlide <- (state.currentSlide + i) % (List.length slides) }
      NoOp      -> state

inputSignal : Signal Input
inputSignal = keyToInput <~ dropRepeats (.x <~ Keyboard.arrows)

keyToInput : Int -> Input
keyToInput x =
  if | x < 0     -> ChangeSlide (-1)
     | x > 0     -> ChangeSlide 1
     | otherwise -> NoOp

renderMain : AppState -> (Int, Int) -> Graphics.Element.Element
renderMain state (w, h) =
  toElement w h <| render state

render : AppState -> Html
render state =
  let
    _ = Debug.watch "currentSlide" state.currentSlide
    stateSlidePairs = List.map ((,) state) slides
  in
    div
      [ class "Presentation" ]
      (List.indexedMap
        renderSlide
        stateSlidePairs)

renderSlide : Int -> (AppState, Slide) -> Html
renderSlide i (state, (title, slide)) =
  div
    [ class "Slide"
    , key title
    , mergeStyles
        [ slideStyle
        , if state.currentSlide == i then activeStyle else noStyle
        ]
    ]
    [ slide ]

-- Styles

mergeStyles : List Style -> Attribute
mergeStyles = (List.foldl Dict.union Dict.empty) >> Dict.toList >> style

slideStyle : Style
slideStyle =
  Dict.fromList
    [ ("position", "absolute")
    , ("box-sizing", "border-box")
    , ("padding", "1em 2.5em")
    , ("width", "100%")
    , ("height", "100%")
    , ("opacity", "0")
    , ("transition", "opacity 300ms ease-in")
    ]

activeStyle : Style
activeStyle =
  Dict.fromList
    [ ("opacity", "1")
    ]

noStyle : Style
noStyle = Dict.empty

-- Slides

slides : List Slide
slides =
  [ title
  , why
  , why2
  , why3
  , agenda
  , assumptions
  , makeSlide "# The Basics"
  , frp
  , makeSlide "![Signal Graph](assets/signal-graph.png)"
  , makeSlide "![Signal](assets/Signal.svg)"
  , makeSlide "![Merge](assets/merge.svg)"
  , makeSlide "![SampleOn](assets/sampleOn.svg)"
  , makeSlide "# The Signals"
  ]

makeSlide : String -> Slide
makeSlide str = (str, Markdown.toHtml str)

title : Slide
title = (,) "title" <| Markdown.toHtml """

# Elm

Tom Ashworth<br>
@phuunet

"""

why : Slide
why = (,) "why" <| Markdown.toHtml """

Learn a new thing & try a new approach to old things.

"""

why2 : Slide
why2 = (,) "why-2" <| Markdown.toHtml """

**Learn a new thing** & try a new approach to old things.

- Functional Reactive Programming
- Strong, static-typing (Hindley-Milner)
- Purely functional language

"""

why3 : Slide
why3 = (,) "why-3" <| Markdown.toHtml """

Learn a new thing & **try a new approach to old things**.

- Front-end application architecture
- Approach to CSS & DOM manipulation
- Excitement about front-end

"""

agenda : Slide
agenda = (,) "agenda" <| Markdown.toHtml """

1. Demo *(Mario)*
2. Basics
3. Demo *(Slugify)*
4. HTTP & JSON
5. Demo *(GitHub Search)*
6. Dojo *(Build a thing)*

"""

assumptions : Slide
assumptions = (,) "assumptions" <| Markdown.toHtml """

I'm going to assume you understand the following:

- pure functions
- mapping, folding/reducing
- higher-order functions and currying

"""

frp : Slide
frp = (,) "frp" <| Markdown.toHtml """

## Functional Reactive Programming

- A *signal-graph* that's *connected input from the world*.
- Signals are *infinite*.
- Inputs are *fixed*.

Elm calls this "first-order" FRP.

"""
