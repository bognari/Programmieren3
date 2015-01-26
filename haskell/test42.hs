add :: Maybe Int -> Maybe Int -> Maybe Int 
add mA mB = case mA of
    Nothing ->  Nothing
    Just a  ->  case mB of
                    Nothing ->  Nothing 
                    Just b  ->  Just(a + b)

add' :: Maybe Int -> Maybe Int -> Maybe Int 
add' mA mB = mA >>= (\a -> mB >>= (\b -> return (a + b)))

add'' :: Maybe Int -> Maybe Int -> Maybe Int 
add'' mA mB = do
        a <- mA
        b <- mB
        return (a + b)