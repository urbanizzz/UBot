module Bot
  ( botMain
  ) where

type BotType = String
type BotToken = String

botMain :: BotType -> BotToken -> IO ()
botMain "vk" token = putStrLn "Launch VKBot"
botMain "tg" token = putStrLn "Launch TGBot"
botMain "cl" _     = putStrLn "Launch Command Line Bot"



