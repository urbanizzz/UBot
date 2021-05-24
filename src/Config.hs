module Config
  ( Config(..)
  , getConfig
  ) where

data Config = Config
  { repeatDefault                       :: Int
  , repeatQuestion                      :: String
  , repeatMaxNumber                     :: Int
  , about                               :: String
  , unknownCommandMessage               :: String
  }

getConfig :: Config
getConfig = Config
  { repeatDefault = 3
  , repeatMaxNumber = 5
  , repeatQuestion = "Enter the number of repetitions"
  , about = "UBot - simple echo bot.\n/help to get this help\n/repeat to set the number of repetition"
  , unknownCommandMessage = "Unknown command: enter /help to get help"
  }

