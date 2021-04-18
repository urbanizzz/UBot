module Types where
{-
  ( BotType
  , BotToken
  , UserName
  , RepeatNumber
  , UsersRepeat
--  , BotState (..)
  , Environment (..)
  ) where
-}

import qualified Data.Map.Lazy as M
import           Control.Monad.State

import Config
import Data.Aeson

type BotType = String
type BotToken = String
type UserName = String
type RepeatNumber = Int
type UsersRepeat = M.Map UserName RepeatNumber
-- newtype BotState a = BotState {runBotState :: State Environment a}

data Environment = Environment
  { botType :: BotType
  , botToken :: BotToken
  , userRepeat :: UsersRepeat
  , config :: Config
  }

data Event = HelpCommand | RepeatCommand | Message Value

