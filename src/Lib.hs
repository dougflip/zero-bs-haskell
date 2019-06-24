module Lib where

import qualified Zero.Server as Server
   
helloHandler :: Server.Request -> Server.Response
helloHandler _ = Server.stringResponse "hello"

echoHandler :: Server.Request -> Server.Response
echoHandler = Server.stringResponse . Server.requestBody

caseHandler :: Server.Request -> Server.Response
caseHandler = Server.stringResponse . mapNumberToString . Server.requestBody

mapNumberToString :: String -> String
mapNumberToString "1" = "one"
mapNumberToString "2" = "two"
mapNumberToString "3" = "three"

run :: IO ()
run = Server.startServer [
    Server.simpleHandler Server.GET "/hello" helloHandler,
    Server.simpleHandler Server.POST "/echo" echoHandler,
    Server.simpleHandler Server.POST "/case" caseHandler
    ]
