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
  , usersRepeat = UsersRepeat M.empty
  , config = getConfig
  }

newHandle :: BotType -> Handle
newHandle (BotType botType) = case botType of
  "vk" -> undefined
  "tg" -> undefined
  "cl" -> Cl.newHandle
  
createBot :: BotType -> BotToken -> IO ()
createBot botType botToken = do
  (a,s) <- (runStateT (mainBot $ newHandle botType)) $ newState botType botToken
  return a

getBotEvent :: Handle -> IO Event
getBotEvent handle = getEvent handle

helpMessage :: Environment -> Value
helpMessage env = stringToValue . about . config $ env

execHelpCommand :: Handle -> EventEscort -> Environment -> IO ()
execHelpCommand handle escort env = do
  let helpMsg = helpMessage $ env
  (sendHelp handle) escort helpMsg

execRepeatCommand handle escort st = undefined --print $ "REPEAT" ++ botType st
repeatMessage :: Handle -> EventEscort -> Environment -> IO ()
repeatMessage handle escort st = replicateM_ num act
  where num = getUserRepeat name st
        name = userName escort
        act = sendMessage handle escort

getUserRepeat :: UserName -> Environment -> Int
getUserRepeat name st = case M.lookup name (unUsersRepeat . usersRepeat $ st) of
  Just rep -> unRepeatNumber rep
  Nothing -> repeatDefault . config $ st

mainBot :: Handle -> StateT Environment IO ()
mainBot handle = forever $ do
  st <- get
  event <- lift $ getBotEvent handle
  case event of
    HelpCommand escort    -> lift $ execHelpCommand handle escort st
    RepeatCommand escort  -> modify (execRepeatCommand handle escort st)
    Message escort        -> lift $ repeatMessage handle escort st

