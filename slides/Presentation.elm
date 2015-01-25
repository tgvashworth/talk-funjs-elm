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
      [ class "Presentation"
      , mergeStyles [ presentationStyle ]
      ]
      [ styleBlock
      , div
          []
          (List.indexedMap
            renderSlide
            stateSlidePairs)
      ]

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

styleBlock : Html
styleBlock =
  node "style"
    []
    [ text "img { width: 100%; }"
    ]

-- Styles

mergeStyles : List Style -> Attribute
mergeStyles = (List.foldl Dict.union Dict.empty) >> Dict.toList >> style

presentationStyle : Style
presentationStyle =
  Dict.fromList
    [ ("font", "24px/1.4 Avenir Next")
    ]

slideStyle : Style
slideStyle =
  Dict.fromList
    [ ("position", "absolute")
    , ("box-sizing", "border-box")
    , ("padding", "1em 2.5em")
    , ("width", "100%")
    , ("max-width", "40em")
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
  , makeSlide "## Demo *(Mario)*"
  , assumptions
  , makeSlide "## The Basics"
  , frp
  , makeSlide "![Signal Graph](assets/signal-graph.svg)"
  , makeSlide "![Signal](assets/Signal.svg)"
  , makeSlide "![Merge](assets/merge.svg)"
  , makeSlide "![SampleOn](assets/sampleOn.svg)"
  , makeSlide "## Signals"
  , makeSlide "## Demo *(Slugify)*"
  , makeSlide "## Types"
  , makeSlide "## Interacting with HTTP and JSON"
  , makeSlide "## Demo *(GitHub Search)*"
  , dojo
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

* Demo *(Mario)*
* Basics
* FRP WTF
* Signals
* Demo *(Slugify)*
* HTTP & JSON
* Demo *(GitHub Search)*
* Dojo *(Build a thing)*

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

dojo : Slide
dojo = (,) "dojo" <| Markdown.toHtml """

## Dojo!

- [elm-lang.org](http://elm-lang.org/)
- Install Elm if you haven't already
- **Build something!** Here's the recipie for Slugify:
  - Get an input box on the screen
  - Hook it up so the value is tied to application state
  - Output the value to a different element
  - Write a cleanup function and apply it to the input box's value before it's rendered
  - Look at [github.com/phuu/talk-funjs-elm](https://github.com/phuu/talk-funjs-elm) for some code

*BTW: this code running this presentation is (reasonably simple) Elm. You could try building that!*

Thanks &hearts;

"""
