-- module of Command Line Bot
module ClBot
  ( getMessage
  , sendMessage
  ) where

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
  )

noneUser = UserName "none"

parseMessage :: String -> Event
parseMessage msg = case (lower . trim $ msg) of
  "/help" -> HelpCommand noneUser
  "/repeat" -> RepeatCommand noneUser
  otherwise -> Message (noneUser) (UserMessage . stringToValue $ msg)

getMessage :: IO Event
getMessage = do
  msg <- getLine
  let event = parseMessage msg
  return event

sendMessage :: Value -> IO ()
sendMessage msg = case (valueToString msg) of
  (Right str) -> putStrLn $ str
  (Left str)  -> putStrLn $ "Error in Cl.sendMessage: " ++ str

