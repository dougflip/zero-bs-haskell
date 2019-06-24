module Lib where

import qualified Zero.Server as Server
   
helloHandler :: Server.Request -> Server.Response
helloHandler _ = Server.stringResponse "hello"

run :: IO ()
run = Server.startServer [
    Server.simpleHandler Server.GET "/hello" helloHandler
    ]
