-- bot logic module
module Bot
  ( createBot
  ) where

import qualified Data.Map.Lazy as M
import qualified Data.ByteString.Char8 as BS
import           Control.Monad.State


type BotType = String
type BotToken = String
type UserName = String
type RepeatNumber = Int
type UsersRepeat = M.Map UserName RepeatNumber

data Config = Config
  { repeatDefault   :: RepeatNumber
  , about           :: String
  , repeatQuestion  :: String
  }

data Environment = Environment
  { botType :: BotType
  , botToken :: BotToken
  , userRepeat :: UsersRepeat
  , config :: Config
  }

newState :: BotType -> BotToken -> Environment
newState btype btoken = Environment
  { botType = btype
  , botToken = btoken
  , userRepeat = M.empty
  , config = Config 3 "UrbanizzzBot" "Enter the number of repetitions"
  }

type BotState = State Environment

createBot :: BotType -> BotToken -> BotState ()
createBot botType botToken = mainBot -- $ newState botType botToken

mainBot :: BotState ()
mainBot = undefined
  









