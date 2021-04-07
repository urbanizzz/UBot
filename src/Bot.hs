-- bot logic module

module Bot
  ( createBot
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Lazy as M

import Types
import Config

newState :: BotType -> BotToken -> Environment
newState btype btoken = Environment
  { botType = btype
  , botToken = btoken
  , userRepeat = M.empty
  , config = getConfig
  }

createBot :: BotType -> BotToken -> BotState ()
createBot botType botToken = mainBot $ newState botType botToken

mainBot :: Environment -> BotState ()
mainBot = undefined
  









