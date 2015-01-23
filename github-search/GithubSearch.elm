module GithubSearch where

import Debug
import Json.Decode as Json
import Json.Decode ((:=))
import Graphics.Element as Element
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Http
import List
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
  toElement w h <|
    div
      [ class "container"
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
      , div
          [ class "users" ]
          (List.indexedMap renderUser state.users)
      ]

renderUser : Int -> GithubUser -> Html
renderUser i user =
  a
    [ href user.url
    , target "blank"
    , userStyle i ]
    [ renderAvatar user
    , text user.login ]

renderAvatar : GithubUser -> Html
renderAvatar user = img [ avatarStyle, src user.avatarUrl ] []

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
    [ ("font-size", "1em")
    , ("width", "100%")
    ]

userStyle : Int -> Attribute
userStyle i =
  style
    [ ("display", "block")
    , ("padding", "0.5em")
    , if i % 2 == 0 then ("background-color", "#fafafa") else ("","")
    ]

avatarStyle : Attribute
avatarStyle =
  style
    [ ("width", "1.5em")
    , ("height", "1.5em")
    , ("display", "inline-block")
    , ("vertical-align", "middle")
    , ("margin-right", "0.5em")
    ]

-- Utils

spacer : String -> Html
spacer h =
  div [ style [ ("height", h) ] ] []
