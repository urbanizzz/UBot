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
getConfig = Config 3 "UrbanizzzBot" "Enter the number of repetitions"

