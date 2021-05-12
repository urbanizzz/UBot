-- module of Command Line Bot
module ClBot
  ( newHandle
  ) where

import qualified Data.Text as T
import qualified Text.Read as Read

import Data.List.Extra (lower,trim)
import Data.Aeson
  ( Value
  )

import Types 
  ( valueToString
  , stringToValue
  , Event (..)
  , UserName (..)
  , UserMessage (..)
  , RepeatNumber (..)
  , Handle (..)
  , EventEscort (..)
  )

noneUser = UserName . T.pack $ "none"
nullMessage = UserMessage . stringToValue $ ""

parseMessage :: String -> Event
parseMessage msg = case (lower . trim $ msg) of
  "/help"   -> HelpCommand $ Escort noneUser nullMessage
  "/repeat" -> RepeatCommand $ Escort noneUser nullMessage
  otherwise -> Message $ Escort (noneUser) (UserMessage . stringToValue $ msg)

{-
data Handle = Handle
  { getEvent    :: Monad m => m Event
  , sendMessage :: Monad m => UserName -> UserMessage -> m ()
  , sendHelp    :: Monad m => UserName -> m ()
  , getRepeat   :: Monad m => UserName -> m ()
  }
-}

newHandle :: Handle
newHandle = Handle
  { getEvent    = getMessageCl
  , sendMessage = sendMessageCl
  , sendHelp    = sendHelpCl
  , getRepeat   = getRepeatCl
  }

getMessageCl :: IO Event
getMessageCl = do
  msg <- getLine
  let event = parseMessage msg
  return event

sendMessageCl :: EventEscort -> IO ()
sendMessageCl escort = 
  case (valueToString . unUserMessage . userMessage $ escort) of
    (Right str) -> putStrLn $ str
    (Left str)  -> putStrLn $ "Error in ClBot.sendMessageCl: " ++ str

sendHelpCl :: EventEscort -> Value -> IO ()
sendHelpCl _ helpMsg = case valueToString helpMsg of
  Left error -> putStrLn $ "Error in ClBot.sendHelpCl: " ++ error
  Right msg  -> putStrLn msg

getRepeatCl :: EventEscort -> RepeatNumber -> Value -> IO RepeatNumber
getRepeatCl _ repeatOld repeatQuestion = do
  case valueToString repeatQuestion of
    Left error  -> do
      putStrLn $ "Error in ClBot.getRepeatCl: " ++ error
      return repeatOld
    Right msg   -> do
      putStrLn msg
      val <- getLine
      case (Read.readMaybe val :: Maybe Int) of
        Nothing -> return repeatOld
        Just rep -> if rep `elem` [1..5]
                      then return $ RepeatNumber rep
                      else return repeatOld

