module Lib where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void, guard)
import System.Process (system, readProcessWithExitCode)
import System.Exit (ExitCode(..), exitFailure)
import Data.List (nub, isInfixOf)
import Data.Maybe (catMaybes, listToMaybe)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

readMaybe :: Read a => String -> Maybe a
readMaybe s = fst <$> listToMaybe (reads s)

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

handleExitCode :: ExitCode -> b -> b -> b
handleExitCode (ExitFailure _) x y = x
handleExitCode ExitSuccess     x y = y

-- cifs can cause issues with the directory package
-- so we make our own
modificationTimeWithTimeout :: Integer -> FilePath -> IO (Maybe Integer)
modificationTimeWithTimeout seconds fp = do
    (ecode,time,_) <- readProcessWithExitCode
      "timeout" [show seconds, "stat", "-c", "%Y", fp] ""
    pure $ handleExitCode ecode Nothing (readMaybe time)


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
      handleExitCode res
        (putStrLn "--- Attempt failed" *> pure (Just i))
        (putStrLn "--- Attempt succeeded" *> pure Nothing)

delayFor :: Integer -> IO ()
delayFor s = threadDelay (fromIntegral s * 1000000)


beb :: IO (Maybe a) -> IO b -> (a -> IO b) -> IO b
beb act fail succeed = do
    res <- act
    case res of
      Nothing -> fail
      Just r -> succeed r

-- watch a file for changes, use it to download videos
-- check file modify time vs last time
runWatcher :: String -> IO ()
runWatcher fp = do
    hSetBuffering stdout NoBuffering
    putStrLn "--- Initial Video Check"
    mod_time' <- modificationTimeWithTimeout 5 fp
    beb (modificationTimeWithTimeout 5 fp)
        (putStrLn "Getting modification time took too long or failed." *> exitFailure)
        (\mod_time -> do
            doEvent fp
            delayFor 7
            putStrLn "--- Initial Delay Ended"
            forever $ loop mod_time
        )
  where
    loop :: Integer -> IO Integer
    loop old_time = do
      mod_time' <- modificationTimeWithTimeout 5 fp
      beb (modificationTimeWithTimeout 5 fp)
          (putStrLn "Getting modification time took too long or failed." *> exitFailure)
          (\new_time -> do
              if new_time > old_time
                then do
                  putStrLn $ "--- File modified at: " ++ show new_time
                  doEvent fp
                  putStrLn "--- Downloading Complete"
                  delayFor 15 *> loop new_time
                else delayFor 15 *> loop old_time
          )
