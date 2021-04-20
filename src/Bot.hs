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
  , userRepeat = M.empty
  , config = getConfig
  }

createBot :: BotType -> BotToken -> IO ()
createBot botType botToken = do
  (a,s) <- (runStateT mainBot) $ newState botType botToken
  return a

getEvent :: Environment -> IO Event
getEvent env = do
  event <- case (botType env) of
    "vk" -> undefined
    "tg" -> undefined
    "cl" -> Cl.getMessage
  return event

helpMessage :: Environment -> Value
helpMessage env = stringToValue . about . config $ env

execHelpCommand :: Environment -> IO ()
execHelpCommand env = do
  let helpMsg = helpMessage $ env
  case (botType env) of
    "vk" -> undefined
    "tg" -> undefined
    "cl" -> Cl.sendMessage $ helpMsg
  

execRepeatCommand st = undefined --print $ "REPEAT" ++ botType st
repeatMessage msg st = undefined --print $ "MESSAGE" ++ botType st ++ "\n" ++ msg


mainBot :: StateT Environment IO ()
mainBot = do
  st <- get
  event <- lift $ getEvent st
  case event of
    HelpCommand   -> lift $ execHelpCommand st
    RepeatCommand -> modify (execRepeatCommand st)
    Message msg   -> repeatMessage msg st









