module Types where
{-
  ( BotType
  , BotToken
  , UserName
  , RepeatNumber
  , UsersRepeat
--  , BotState (..)
  , Environment (..)
  ) where
-}

import qualified Data.Map.Lazy as M
import qualified Data.Text as T

import Control.Monad.State
import Data.Aeson (Value(..))

import Config

newtype BotType       = BotType       {unBotType      :: String}
newtype BotToken      = BotToken      {unBotToken     :: String}
newtype UserName      = UserName      {unUserName     :: String}
newtype RepeatNumber  = RepeatNumber  {unRepeatNumber :: Int}
newtype UserRepeat    = UserRepeat    {unUserRepeat   :: M.Map UserName RepeatNumber}
newtype UserMessage   = UserMessage   {unUserMessage  :: Value}
-- newtype BotState a = BotState {runBotState :: State Environment a}

data Environment = Environment
  { botType :: BotType
  , botToken :: BotToken
  , userRepeat :: UserRepeat
  , config :: Config
  }

data Event  = HelpCommand UserName
            | RepeatCommand UserName
            | Message UserName UserMessage

stringToValue :: String -> Value
stringToValue str = String $ T.pack str

valueToString :: Value -> Either String String
valueToString (String str) = Right . T.unpack $ str
valueToString  _ = Left "Value is not (String a)"

