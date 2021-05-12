module Types where

import qualified Data.Map.Lazy as M
import qualified Data.Text as T

import Control.Monad.State
import Data.Aeson (Value(..))

import Config

newtype BotType       = BotType       {unBotType      :: String}
newtype BotToken      = BotToken      {unBotToken     :: String}
newtype UserName      = UserName      {unUserName     :: T.Text}
  deriving (Eq, Ord)
newtype RepeatNumber  = RepeatNumber  {unRepeatNumber :: Int}
newtype UsersRepeat   = UsersRepeat   {unUsersRepeat  :: M.Map UserName RepeatNumber}
newtype UserMessage   = UserMessage   {unUserMessage  :: Value}

data Environment = Environment
  { botType     :: BotType
  , botToken    :: BotToken
  , usersRepeat  :: UsersRepeat
  , config      :: Config
  }

data Event  = HelpCommand EventEscort
            | RepeatCommand EventEscort
            | Message EventEscort

data EventEscort = Escort
  { userName    :: UserName
  , userMessage :: UserMessage
  }

-- переделать с IO на Monad m
data Handle = Handle
  { getEvent    :: IO Event
  , sendMessage :: EventEscort -> IO ()
  , sendHelp    :: EventEscort -> Value -> IO ()
  , getRepeat   :: EventEscort -> RepeatNumber -> Value -> IO RepeatNumber
  }

stringToValue :: String -> Value
stringToValue str = String $ T.pack str

valueToString :: Value -> Either String String
valueToString (String str) = Right . T.unpack $ str
valueToString  _ = Left "Value is not (String a)"

