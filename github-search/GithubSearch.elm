module GithubSearch where

import Debug
import Graphics.Element as Element
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Http
import Regex
import Signal (..)
import Signal.Time
import String
import Window

-- Model

type alias State =
  { username : String }

type Update
  = NoOp
  | Change String

type QueryResponse
  = QuerySuccess String
  | QueryWaiting
  | QueryFailure

-- Main

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
            , on "keyup" targetValue (send queryChannel)
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

-- Inputs

updates : Channel Update
updates = channel NoOp

queryChannel : Channel String
queryChannel = channel ""

querySignal : Signal String
querySignal = subscribe queryChannel |> dropRepeats

throttledQuerySignal : Signal String
throttledQuerySignal = Signal.Time.limitRate 1 querySignal

queryResponseSignal : Signal QueryResponse
queryResponseSignal = processQueryHttpResponse <~ Http.sendGet (makeQueryUrl <~ throttledQuerySignal)

makeQueryUrl : String -> String
makeQueryUrl query =
  let
    _ = Debug.watch "query" query
  in
    case query of
      "" -> ""
      _ -> "https://api.github.com/search/users?access_token=2cf5002b767faf4aae3a3645952d8d24e7eecc64&q=" ++ query

processQueryHttpResponse : Http.Response String -> QueryResponse
processQueryHttpResponse res =
  let
    _ = Debug.watch "res" res
  in
    case res of
      Http.Success result -> QuerySuccess "success!"
      Http.Waiting -> QueryWaiting
      Http.Failure _ _ -> QueryFailure

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
