module Main where

import System.Environment (getArgs)
import Control.Monad (when,guard)

errPrint :: String -> IO ()
errPrint err = putStrLn $ err ++ " Use\nUBot {vk,tg} <token>"

checkBotType :: String -> Bool
checkBotType "vk" = True
checkBotType "tg" = True
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
  putStrLn $ "launch: bot " ++ botType ++ " " ++ botToken

