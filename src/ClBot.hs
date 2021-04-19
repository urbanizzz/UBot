-- module of Command Line Bot
module ClBot
  ( getMessage
  ) where

import Data.List.Extra (lower,trim)
import Types (Event (..))
import qualified Data.Aeson as A
import qualified Data.Text as T

parseMessage :: String -> Event
parseMessage msg = case (lower . trim $ msg) of
  "/help" -> HelpCommand
  "/repeat" -> RepeatCommand
  otherwise -> Message $ A.String $ T.pack msg

getMessage :: IO Event
getMessage = do
  msg <- getLine
  let event = parseMessage msg
  return event
