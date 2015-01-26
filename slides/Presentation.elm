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

-- Ports

port urlSlideInput : Signal Int

port urlSlide : Signal Int
port urlSlide = .currentSlide <~ appStateSignal

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
  | SetSlide Int

type alias Slide = (String, Html)

type alias Style = Dict.Dict String String

-- Inputs

inputSignal : Signal Input
inputSignal =
  Signal.merge
    (keyToInput <~ dropRepeats (.x <~ Keyboard.arrows))
    (SetSlide <~ urlSlideInput)


keyToInput : Int -> Input
keyToInput x =
  if | x < 0     -> ChangeSlide (-1)
     | x > 0     -> ChangeSlide 1
     | otherwise -> NoOp

-- Update

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
      SetSlide i    -> { state | currentSlide <- i }
      NoOp          -> state

-- View

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
    [ text """
    img { width: 100%; }
    .small-img { max-width: 20em;}
    #logo { height: 1em; width: 1em; }
    a:link { text-decoration: none; }
    a:hover { border-bottom: 1px solid currentColor; }
    h1 { margin-bottom: 0; }
    h4 { margin-top: 0; font-weight: normal; }
    """
    ]

-- Styles

mergeStyles : List Style -> Attribute
mergeStyles = (List.foldl Dict.union Dict.empty) >> Dict.toList >> style

presentationStyle : Style
presentationStyle =
  Dict.fromList
    [ ("font", "30px/1.4 Avenir Next")
    , ("overflow", "hidden")
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
    , ("z-index", "1")
    ]

blackStyle : Style
blackStyle =
  Dict.fromList
    [ ("position", "absolute")
    , ("top", "-1000px")
    , ("bottom", "-1000px")
    , ("left", "-1000px")
    , ("right", "-1000px")
    , ("background", "black")
    ]

noStyle : Style
noStyle = Dict.empty

-- Slides

slides : List Slide
slides =
  [ title
  , why2
  , why3
  , agenda
  , assumptions
  , makeSlide <|
      "### Demo *([Mario.elm](http://debug.elm-lang.org/edit/Mario.elm))*\n" ++
      "<img class=small-img src=http://media.giphy.com/media/a7WK2JbSFb05i/giphy.gif>"
  , evan
  , makeSlide "### Basics.elm"
  , frp
  , makeSlide "![Signal Graph](assets/signal-graph.svg)"
  , makeSlide "![Signal](assets/Signal.svg)"
  , makeSlide "![map](assets/map.svg)"
  , makeSlide "![Merge](assets/merge.svg)"
  , makeSlide "![SampleOn](assets/sampleOn.svg)"
  , makeSlide "### Signals.elm"
  , makeSlide "### Demo *(Slugify.elm)*"
  , makeSlide "### Types.elm"
  , makeSlide "### HTTPAndJSON.elm"
  , makeSlide "### Demo *(GitHubSearch.elm)*"
  , dojo
  , black
  ]

makeSlide : String -> Slide
makeSlide str = (str, Markdown.toHtml str)

black : Slide
black = (,) "" <|
  div
    [ mergeStyles [ blackStyle ] ]
    []

title : Slide
title = (,) "title" <| Markdown.toHtml """

# <img id="logo" src="http://elm-lang.org/logo.svg"> Elm

#### *Learning to love the signal graph.*

[github.com/phuu/talk-funjs-elm](https://github.com/phuu/talk-funjs-elm)

Tom Ashworth<br>
@phuunet

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

I'm going to assume you understand:

- pure functions
- mapping, folding/reducing
- higher-order functions and currying
- homotopy type theory

<img class=small-img src=http://media.giphy.com/media/Qmt4gXP40kmeA/giphy.gif>"

"""

evan : Slide
evan = (,) "evan" <| Markdown.toHtml """

# Evan Czaplicki
#### @czaplic

<img class=small-img src=https://pbs.twimg.com/profile_images/443794371586977792/NxKUNpOQ_400x400.jpeg>"

"""

frp : Slide
frp = (,) "frp" <| Markdown.toHtml """

### Functional Reactive Programming

- A *signal-graph* that's *connected to input from the world*.
- Signals are *infinite*.
- Inputs are *fixed*.

Elm calls this "first-order" FRP.

"""

dojo : Slide
dojo = (,) "dojo" <| Markdown.toHtml """

### Dojo!

- Install Elm if you haven't already
- **Build something!** Here's the recipie for Slugify:
  - Get an input box on the screen & hook it up to application state
  - Output the value to a different element
  - Write a cleanup function and apply it to the input box's value before it's rendered

### Useful links

- [elm-lang.org](http://elm-lang.org/) — Elm's website
- [package.elm-lang.org](http://package.elm-lang.org/) — Elm's package manager
- [github.com/phuu/talk-funjs-elm](https://github.com/phuu/talk-funjs-elm) — This presentation and examples

*BTW: this code running this presentation is (reasonably simple) Elm. You could try building that!*

Thanks &hearts;

"""
