module Echo where

import System.IO

echo :: String -> IO ()
echo w = do
  putStrLn "\ESC[2J\n"
  putStrLn w
  c <- getChar
  case c of 
    '\DEL' -> if null w 
        then echo w
        else echo $ tail w
    '\ESC' -> do
          putStrLn "\ESC[2J\n"
          return ()
    _      -> echo $ c : w 

main = do
  hSetBuffering stdin NoBuffering
  echo ""