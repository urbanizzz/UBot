-- bot logic module

module Bot
  ( createBot
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Lazy as M
import qualified ClBot as Cl


import Control.Monad.State

import Types
import Config
import Data.Aeson (Value)

newState :: BotType -> BotToken -> Environment
newState btype btoken = Environment
  { botType = btype
  , botToken = btoken
  , userRepeat = UserRepeat M.empty
  , config = getConfig
  }

createBot :: BotType -> BotToken -> IO ()
createBot botType botToken = do
  (a,s) <- (runStateT mainBot) $ newState botType botToken
  return a

getBotEvent :: Environment -> IO Event
getBotEvent env = do
  event <- case (unBotType . botType $ env) of
    "vk" -> undefined
    "tg" -> undefined
    "cl" -> Cl.getMessageCl
  return event

helpMessage :: Environment -> Value
helpMessage env = stringToValue . about . config $ env

execHelpCommand :: EventEscort -> Environment -> IO ()
execHelpCommand escort env = do
  let helpMsg = helpMessage $ env
  case (unBotType . botType $ env) of
    "vk" -> undefined
    "tg" -> undefined
    "cl" -> Cl.sendMessageCl helpMsg
  

execRepeatCommand escort st = undefined --print $ "REPEAT" ++ botType st
repeatMessage escort st     = undefined --print $ "MESSAGE" ++ botType st ++ "\n" ++ msg


mainBot :: StateT Environment IO ()
mainBot = do
  st <- get
  event <- lift $ getBotEvent st
  case event of
    HelpCommand escort    -> lift $ execHelpCommand escort st
    RepeatCommand escort  -> modify (execRepeatCommand escort st)
    Message escort        -> repeatMessage escort st









