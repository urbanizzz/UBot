-- module of Command Line Bot
module ClBot
  ( getMessage
  , sendMessage
  ) where

import Data.List.Extra (lower,trim)
import Types (valueToString, stringToValue, Event (..))
import Data.Aeson (Value)

parseMessage :: String -> Event
parseMessage msg = case (lower . trim $ msg) of
  "/help" -> HelpCommand
  "/repeat" -> RepeatCommand
  otherwise -> Message $ stringToValue $ msg

getMessage :: IO Event
getMessage = do
  msg <- getLine
  let event = parseMessage msg
  return event

sendMessage :: Value -> IO ()
sendMessage msg = case (valueToString msg) of
  (Right str) -> putStrLn $ str
  (Left str)  -> putStrLn $ "Error in Cl.sendMessage: " ++ str

