module Main (main) where

import Network.Fihles.Server
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let port = if null args then 8080 else read $ head args
  runServer port
