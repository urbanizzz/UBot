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

getEvent :: Environment -> IO Event
getEvent env = do
  event <- case (unBotType . botType $ env) of
    "vk" -> undefined
    "tg" -> undefined
    "cl" -> Cl.getMessage
  return event

helpMessage :: Environment -> Value
helpMessage env = stringToValue . about . config $ env

execHelpCommand :: UserName -> Environment -> IO ()
execHelpCommand userName env = do
  let helpMsg = helpMessage $ env
  case (unBotType . botType $ env) of
    "vk" -> undefined
    "tg" -> undefined
    "cl" -> Cl.sendMessage helpMsg
  

execRepeatCommand userName st = undefined --print $ "REPEAT" ++ botType st
repeatMessage msg userName st = undefined --print $ "MESSAGE" ++ botType st ++ "\n" ++ msg


mainBot :: StateT Environment IO ()
mainBot = do
  st <- get
  event <- lift $ getEvent st
  case event of
    HelpCommand userName    -> lift $ execHelpCommand userName st
    RepeatCommand userName  -> modify (execRepeatCommand userName st)
    Message userName msg    -> repeatMessage userName msg st









