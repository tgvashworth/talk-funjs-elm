module GithubSearch where

import Debug
import Json.Decode as Json
import Json.Decode ((:=))
import Graphics.Element as Element
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Http
import Regex
import Result
import Signal (..)
import Signal.Time
import Time
import String
import Window

-- Model

type alias State =
  { query : String
  , users : List GithubUser }

type Update
  = NoOp
  | Query String
  | Users (List GithubUser)

type QueryResponse a
  = QuerySuccess a
  | QueryWaiting
  | QueryFailure

type alias GithubUser =
  { login : String
  , id : Int
  , avatarUrl : String
  , url : String
  }

type alias GithubUsers = List GithubUser

-- Main

main : Signal Element.Element
main = render <~ state
               ~ Window.dimensions

state : Signal State
state = foldp step initialState updates

render : State -> (Int,Int) -> Element.Element
render state (w,h) =
  let
    _ = Debug.watch "state.query" state.query
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
            , value state.query
            ]
            []
        , spacer "1em"
        ]

step : Update -> State -> State
step update state =
  let
    _ = Debug.watch "update" update
  in
    case update of
      Users users -> { state | users <- users }
      Query query -> { state | query <- query }
      _ -> state

initialState : State
initialState =
  { query = ""
  , users = [] }

-- Inputs

updates : Signal Update
updates =
  mergeMany
    [ Time.delay Time.millisecond queryResponseSignal
    , Query <~ (subscribe queryChannel) ]

queryChannel : Channel String
queryChannel = channel ""

throttledQuerySignal : Signal String
throttledQuerySignal = Signal.Time.limitRate 1 (subscribe queryChannel)

queryResponseSignal : Signal Update
queryResponseSignal = processQueryHttpResponse <~ Http.sendGet (makeQueryUrl <~ throttledQuerySignal)

makeQueryUrl : String -> String
makeQueryUrl query =
  let
    _ = Debug.watch "query" query
  in
    case query of
      "" -> ""
      _ -> "https://api.github.com/search/users?access_token=2cf5002b767faf4aae3a3645952d8d24e7eecc64&q=" ++ query

processQueryHttpResponse : Http.Response String -> Update
processQueryHttpResponse res =
  case res of
    Http.Success result -> Users (processQueryResponseJson result)
    _ -> NoOp

processQueryResponseJson : String -> GithubUsers
processQueryResponseJson json =
  case (Json.decodeString queryResponseDecoder json) of
    Result.Ok githubUsers -> githubUsers
    _ -> []

queryResponseDecoder : Json.Decoder (List GithubUser)
queryResponseDecoder =
  Json.at [ "items" ] (Json.list githubUserDecoder)

githubUserDecoder : Json.Decoder GithubUser
githubUserDecoder =
  Json.object4 GithubUser
    ("login"      := Json.string)
    ("id"         := Json.int)
    ("avatar_url" := Json.string)
    ("html_url"   := Json.string)

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
