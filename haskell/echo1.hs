module Echo where

import System.IO

echo :: String -> IO ()
echo w = do
  putStrLn w
  c <- getChar
  echo $ w ++ [c]

main = do
  hSetBuffering stdin NoBuffering
  echo ""