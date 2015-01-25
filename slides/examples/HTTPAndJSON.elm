-- Making an GET Request

httpResponseSignal : Signal Http.Response
httpResponseSignal = Http.sendGet (inputToUrl <~ inputSignal)

inputToUrl : String -> String
inputToUrl input = "https://api.example.com/search?q=" ++ input

-- Decoding the result

searchResults : Signal (Maybe (List SearchResult))
searchResults = decodeHttpResponse <~ httpResponseSignal

decodeHttpResponse Http.Response ~> Maybe (List SearchResult)
decodeHttpResponse res =
  case res of
    Http.Success json -> decodeJson json
    _                 -> Nothing

decodeJson String -> Maybe (List SearchResult)
decodeJson json =
  case (Json.decodeString responseDecoder json) of
    Result.Ok searchResults -> Just searchResults
    _                       -> Nothing

responseDecoder : Json.Decoder (List SearchResult)
responseDecoder =
  Json.at [ "results" ] (Json.list searchResultDecoder)

searchResultDecoder : Json.Decoder SearchResult
searchResultDecoder =
  Json.object2 SearchResult
    ("name" := Json.string)
    ("url"  := Json.int)
