module Main where

import Parser
import Eval
import Control.Monad.IO.Class
import Control.Monad ((>=>))
import System.Console.Haskeline

process :: String -> IO ()
process line = either print print $ do 
  parseExpr line >>= runEval

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = getInputLine "Happy> " >>= maybe (outputStrLn "Goodbye.") (liftIO . process >=> (const loop))
