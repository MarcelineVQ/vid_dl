module Lib where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void, guard)
import System.Process (system, readProcessWithExitCode, delegate_ctlc, waitForProcess, createProcess_, proc, std_out, std_err, StdStream(..))
import System.Exit (ExitCode(..), exitFailure)
import Data.List (nub, isInfixOf)
import Data.Maybe (catMaybes, listToMaybe)
import System.IO (hSetBuffering, BufferMode(..), stdout, hGetContents)

import Control.Concurrent

readMaybe :: Read a => String -> Maybe a
readMaybe s = fst <$> listToMaybe (reads s)

-- Not sure what's best universally.
-- A dot + a slash are probably enough to cover most sites
-- and elimnate most bad cases.
isLink :: String -> Bool
isLink url = "." `isInfixOf` url && "/" `isInfixOf` url

handleExitCode :: ExitCode -> b -> b -> b
handleExitCode (ExitFailure _) x y = x
handleExitCode ExitSuccess     x y = y

-- cifs can cause issues with the directory package
-- so we make our own modification time checker
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
        coms = map (\x -> (ytcmd False x, x)) links
    fails1 <- catMaybes <$> mapM runCmd coms
    -- retry failures
    fails2 <- catMaybes <$> mapM runCmd fails1
    -- one last try
    mapM_ runCmd fails2
  where
    -- return failures so we can retry them
    runCmd :: (IO CmdResult, String) -> IO (Maybe (IO CmdResult, String))
    runCmd i@(com,name) = do
      putStrLn $ "--- Attempting download of: " ++ name
      res <- com
      case res of
        Good -> putStrLn "--- Attempt succeeded" *> pure Nothing
        FormatFailure -> (putStrLn "--- Format not available" *> pure (Just (ytcmd True name,name)))
        OtherError _ -> (putStrLn "--- Attempt failed" *> pure (Just i))

delayFor :: Integer -> IO ()
delayFor s = threadDelay (fromIntegral s * 1000000)

data CmdResult = Good | FormatFailure | OtherError ExitCode deriving (Show)
cmd :: (String,[String]) -> IO CmdResult
cmd (com,opts) = do
    (_,Just hout,Just herr,p) <- createProcess_ "system" (proc com opts) { delegate_ctlc = True, std_out = CreatePipe, std_err = CreatePipe}
    out <- hGetContents hout
    err <- hGetContents herr
    forkIO $ hSetBuffering stdout LineBuffering *> putStrLn out
    exit <- waitForProcess p

    pure $ case exit of
      ExitSuccess -> Good
      errR -> if isFormatError err
        then FormatFailure
        else OtherError errR
  where
    isFormatError = any ("format not available"`isInfixOf`) . lines

-- take a url and make it a shell command
ytcmd :: Bool -> String -> IO CmdResult
ytcmd retry url = cmd ("youtube-dl",opts)
  where
    opts = ["-f", format, "--recode-video", "mp4", "--no-playlist", "--newline", url]
    format = if retry then "best" else "bestvideo+bestaudio"

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
