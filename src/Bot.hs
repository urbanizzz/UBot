-- bot logic module

module Bot
  ( createBot
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Lazy as M
import qualified ClBot as CL


import Control.Monad.State

import Types
import Config

newState :: BotType -> BotToken -> Environment
newState btype btoken = Environment
  { botType = btype
  , botToken = btoken
  , userRepeat = M.empty
  , config = getConfig
  }

createBot :: BotType -> BotToken -> ()
createBot botType botToken = fst . runState mainBot $ newState botType botToken

getEvent :: Environment -> IO Event
getEvent env = do
  event <- case (botType env) of
    "vk" -> undefined
    "tg" -> undefined
    "cl" -> CL.getMessage
  return event



execHelpCommand st = undefined
execRepeatCommand st = undefined
repeatMessage msg st = undefined

mainBot :: State Environment ()
mainBot = do
  st <- get
  event <- getEvent st
  case event of
    HelpCommand   -> execHelpCommand st
    RepeatCommand -> modify (execRepeatCommand st)
    Message msg   -> repeatMessage msg st









