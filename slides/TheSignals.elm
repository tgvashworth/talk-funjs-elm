module TheSignals where

-- Mapping

main = Signal.map render appState

main = render <~ appState ~ Window.dimensions

render : AppState -> (Int, Int) -> Html
render state (w,h) = ...

-- Inputs

queryChannel : Channel String
queryChannel = channel ""

render = input [ on "change" targetValue (send queryChannel) ] []
