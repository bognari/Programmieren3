isInfix :: String -> String -> Maybe String
isInfix [] _ = Nothing
isInfix t w | w == take (length w) t = Just t
            | otherwise = isInfix (tail t) w

main = do
  putStrLn "Dateipfad: "
  filepath <- getLine
  putStrLn "gesuchtes Wort: "
  w <- getLine
  c <- readFile filepath
  case (isInfix c w) of
    Nothing -> putStrLn "nicht enthalten"
    Just s -> putStrLn $
        "an Stelle " ++ (take 100 s)