module Lib where

import qualified Data.List   as List
import qualified Ex06
import qualified Ex08
import qualified Zero.Server as Server

data OnOffState
  = On
  | Off
  deriving (Show, Eq)

helloHandler :: Server.Request -> Server.Response
helloHandler _ = Server.stringResponse "hello"

echoHandler :: Server.Request -> Server.Response
echoHandler = Server.stringResponse . Server.requestBody

caseHandler :: Server.Request -> Server.Response
caseHandler = Server.stringResponse . mapNumberToString . Server.requestBody

stringManipulationHandler :: Server.Request -> Server.Response
stringManipulationHandler =
  Server.stringResponse . manipulateString . Server.requestBody

onOffHandler :: OnOffState -> Server.Request -> (OnOffState, Server.Response)
onOffHandler state _ = (newState, Server.stringResponse (show newState))
  where
    newState =
      case state of
        On  -> Off
        Off -> On

mapNumberToString :: String -> String
mapNumberToString "1" = "one"
mapNumberToString "2" = "two"
mapNumberToString "3" = "three"

manipulateString :: String -> String
manipulateString s =
  case List.stripPrefix "I'm positive" s of
    Nothing      -> s
    Just message -> "I think" ++ message

run :: IO ()
run =
  Server.startServer
    [ Server.simpleHandler Server.GET "/hello" helloHandler
    , Server.simpleHandler Server.POST "/echo" echoHandler
    , Server.simpleHandler Server.POST "/case" caseHandler
    , Server.simpleHandler
        Server.POST
        "/string-manipulation"
        stringManipulationHandler
    , Server.handlersWithState
        Off
        [Server.statefulHandler Server.POST "/onoff-switch" onOffHandler]
    , Server.handlersWithState
        Ex06.getInitialState
        [ Server.statefulHandler
            Server.GET
            "/current-count"
            Ex06.currentCountHandler
        , Server.statefulHandler Server.POST "/increase" Ex06.increaseHandler
        ]
    , Server.handlersWithState
        Ex08.getInitialState
        [ Server.statefulHandler Server.GET "/cart" Ex08.getCartHandler
        , Server.statefulHandler Server.POST "/cart" Ex08.updateCartHandler
        ]
    ]
