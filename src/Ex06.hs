module Ex06
  ( getInitialState
  , currentCountHandler
  , increaseHandler
  ) where

import qualified Zero.Server as Server

newtype State =
  State Int
  deriving (Show, Eq)

getInitialState :: State
getInitialState = State 0

currentCountHandler :: State -> Server.Request -> (State, Server.Response)
currentCountHandler state _ = (state, Server.stringResponse (show count))
  where
    State count = state

increaseHandler :: State -> Server.Request -> (State, Server.Response)
increaseHandler (State count) _ =
  (State $ count + 1, Server.stringResponse "ok")
