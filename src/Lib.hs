module Lib where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import System.Process (system)
import System.Exit (ExitCode(..))
import Data.List (nub, isInfixOf)
import Data.Maybe (catMaybes)
import System.Directory (getModificationTime)
import Data.Time.Clock (UTCTime)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Control.Concurrent.Async --(race)

-- take a url and make it a shell command
assembleCommand :: String -> String
assembleCommand = (yt <>) . sorround
  where
    yt = "youtube-dl -f \"best\" --no-playlist --newline "
    sorround s = "\"" <> s <> "\""

-- Not sure what's best universally.
-- A dot + a slash are probably enough to cover most sites
-- and elimnate most bad cases.
isLink :: String -> Bool
isLink url = "." `isInfixOf` url && "/" `isInfixOf` url


-- do downloads
doEvent :: String -> IO ()
doEvent fp = do
    c <- nub . words <$> readFile fp
    let links = filter isLink c
        coms = map (\x -> (assembleCommand x, x)) links
    fails1 <- catMaybes <$> mapM runCmd coms
    -- retry failures
    fails2 <- catMaybes <$> mapM runCmd fails1
    -- one last try
    mapM_ runCmd fails2
  where
    -- return failures so we can retry them
    runCmd :: (String, String) -> IO (Maybe (String, String))
    runCmd i@(com,name) = do
      putStrLn $ "--- Attempting download of: " ++ name
      res <- system com
      case res of
        ExitFailure _ ->
           putStrLn "--- Attempt failed" *> pure (Just i)
        ExitSuccess ->
          putStrLn "--- Attempt succeeded" *> pure Nothing


-- watch a file for changes, use it to download videos
-- check file modify time vs last time
-- cpu use of getModificationTime can spiral if the cifs mounted
-- file being watched is disconnected so we have a timeout
runWatcher :: String -> IO ()
runWatcher fp = do
    hSetBuffering stdout NoBuffering
    putStrLn "--- Initial Video Check"
    -- this doesn't actually complete the race if getModificationTime
    -- is accessing a mounted cifs partition who's remote pc is off
    mod_time' <- race timeOut (getModificationTime fp)
    case mod_time' of
      Left _ -> putStrLn "getModificationTime took too long"
      Right mod_time -> do
        doEvent fp
        threadDelay 7000000 -- wait some time, no need to check quickly
        putStrLn "--- Initial Delay Ended"
        forever $ loop mod_time
  where
    timeOut :: IO ()
    timeOut = threadDelay 10000000
    loop :: UTCTime -> IO UTCTime
    loop old_time = do
      new_time <- getModificationTime fp
      if new_time > old_time
        then do
          putStrLn $ "--- File modified at: " ++ show new_time
          doEvent fp
          putStrLn "--- Downloading Complete"
          threadDelay 15000000 *> loop new_time
        else threadDelay 15000000 *> loop old_time

-- bad
raceTest :: IO (Either () Char)
raceTest = race (threadDelay 2000000) (forever $ pure "c")

-- bad
raceTest' :: IO (Either () Char)
raceTest' = do
    withAsync (threadDelay 2000000)
      (\a -> withAsync (forever $ pure "c")
        (\b -> waitEitherCancel a b))

-- fine maybe due to threadDelay having to be executed where pure could be lifted out
raceTest'' :: IO (Either () Char)
raceTest'' = race (threadDelay 2000000) (forever $ threadDelay 1 *> pure "c")



-- strace -f -p pid
-- -f is for following spawned threads
-- try +RTS -V0
-- to reduce RTS noise
