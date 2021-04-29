module Config
  ( Config(..)
  , getConfig
  ) where

data Config = Config
  { repeatDefault   :: Int
  , about           :: String
  , repeatQuestion  :: String
  }

getConfig :: Config
getConfig = Config 3 "UBot - simple echo bot" "Enter the number of repetitions"

