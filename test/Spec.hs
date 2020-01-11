import Lib

main :: IO ()
main = do
    r <- raceTest''
    case r of
      Left l -> pure l
      Right r -> putChar r
