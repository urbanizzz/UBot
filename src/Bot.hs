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
helpMessage = stringToValue . about . config

execHelpCommand :: Handle -> EventEscort -> Environment -> IO ()
execHelpCommand handle escort env = do
  let helpMsg = helpMessage $ env
  (sendHelp handle) escort helpMsg

repeatQuestionText :: Environment -> Value
repeatQuestionText = stringToValue . repeatQuestion . config

execRepeatCommand :: Handle -> EventEscort -> StateT Environment IO ()
execRepeatCommand handle escort = do
  st <- get
  let name = getUserName escort
  let repeat = getUserRepeat name $ st
  let repeatMsg = repeatQuestionText st
  let repeatMap = unUsersRepeat . usersRepeat $ st
  numberOfRepeat <- lift $ (getRepeat handle) escort repeat repeatMsg
  let newRepeatMap = M.insert name numberOfRepeat repeatMap
  let stNew = st {usersRepeat = UsersRepeat newRepeatMap}
  put stNew

repeatMessage :: Handle -> EventEscort -> Environment -> IO ()
repeatMessage handle escort st = replicateM_ num act
  where num = unRepeatNumber . getUserRepeat name $ st
        name = getUserName escort
        act = sendMessage handle escort

getUserName :: EventEscort -> UserName
getUserName = userName

getUserRepeat :: UserName -> Environment -> RepeatNumber
getUserRepeat name st = case M.lookup name (unUsersRepeat . usersRepeat $ st) of
  Just rep -> rep
  Nothing -> RepeatNumber . repeatDefault . config $ st

mainBot :: Handle -> StateT Environment IO ()
mainBot handle = forever $ do
  st <- get
  event <- lift $ getBotEvent handle
  case event of
    HelpCommand escort    -> lift $ execHelpCommand handle escort st
    RepeatCommand escort  -> execRepeatCommand handle escort
    Message escort        -> lift $ repeatMessage handle escort st

