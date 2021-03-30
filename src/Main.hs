-- TODO избавиться от guard, он дает лишнее сообщение об ошибке
module Main where

import System.Environment (getArgs)
import Control.Monad (when,guard)

import Bot

errPrint :: String -> IO ()
errPrint err = putStrLn $ err ++ " Use\nUBot {vk,tg,cl} <token>"

checkBotType :: String -> Bool
checkBotType "vk" = True
checkBotType "tg" = True
checkBotType "cl" = True
checkBotType _ = False

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) $ errPrint $ "Bot type is not specified."
  guard (length args > 0)
  let (botType:rest) = args
  when (not . checkBotType $ botType) $ errPrint "Bot type is wrong." 
  guard (checkBotType botType)
  when (length rest == 0) $ errPrint $ "Bot token is not specified."
  guard (length rest > 0)
  let botToken = head rest
  botMain botType botToken

