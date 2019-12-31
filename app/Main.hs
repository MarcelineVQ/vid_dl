module Main where

import Lib

import System.Environment

main :: IO ()
main = do
  (x:_) <- getArgs -- file to watch for urls
  runWatcher x
