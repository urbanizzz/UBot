module Types where

import qualified Data.Map.Lazy as M
import           Control.Monad.State

import Config

type BotType = String
type BotToken = String
type UserName = String
type RepeatNumber = Int
type UsersRepeat = M.Map UserName RepeatNumber
type BotState = State Environment

data Environment = Environment
  { botType :: BotType
  , botToken :: BotToken
  , userRepeat :: UsersRepeat
  , config :: Config
  }

