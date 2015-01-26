module Caesar where

import System.IO

echo :: String -> Int -> IO ()
echo w x = do
  putStrLn "\ESC[2J\n"
  putStrLn w
  c <- getChar >>= (return.shift)
  case c of 
    '\DEL' -> if null w 
                then echo w x
                else echo (init w) x
    '\ESC' -> do
                putStrLn "\ESC[2J\n"
                return ()
    _      -> echo (w ++ [c]) x
  where
    shift ch = if ((fromEnum ch) >= 32 && (fromEnum ch) <= 126)
                then (toEnum $ s ch) :: Char
                else ch    
    s = (+)32.(flip mod) 96.(+)x.(flip (-)) 32.fromEnum

main x = do
  hSetBuffering stdin NoBuffering
  echo "" x