module Lib where

import qualified Data.List as List
import qualified Zero.Server as Server
   
helloHandler :: Server.Request -> Server.Response
helloHandler _ = Server.stringResponse "hello"

echoHandler :: Server.Request -> Server.Response
echoHandler = Server.stringResponse . Server.requestBody

caseHandler :: Server.Request -> Server.Response
caseHandler = Server.stringResponse . mapNumberToString . Server.requestBody

stringManipulationHandler :: Server.Request -> Server.Response
stringManipulationHandler = Server.stringResponse . manipulateString . Server.requestBody

onOffHandler :: Bool -> Server.Request -> (Bool, Server.Response)
onOffHandler isOn req = (newState, Server.stringResponse response)
    where
        newState = if isOn then False else True
        response = if newState then "On" else "Off"

mapNumberToString :: String -> String
mapNumberToString "1" = "one"
mapNumberToString "2" = "two"
mapNumberToString "3" = "three"

manipulateString :: String -> String
manipulateString s =
    case List.stripPrefix "I'm positive" s of
        Nothing -> s
        Just message -> "I think" ++ message

run :: IO ()
run = Server.startServer [
    Server.simpleHandler Server.GET "/hello" helloHandler,
    Server.simpleHandler Server.POST "/echo" echoHandler,
    Server.simpleHandler Server.POST "/case" caseHandler,
    Server.simpleHandler Server.POST "/string-manipulation" stringManipulationHandler,
    Server.handlersWithState False [Server.statefulHandler Server.POST "/onoff-switch" onOffHandler]
    ]
