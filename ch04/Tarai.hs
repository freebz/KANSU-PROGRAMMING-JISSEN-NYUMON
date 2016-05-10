-- Tarai.hs
tarai :: Int -> Int -> Int -> Int
tarai x y z
    | x <= y = y
    | otherwise = tarai
                  (tarai (x - 1) y z)
                  (tarai (y - 1) z x)
                  (tarai (z - 1) x y)

taraiLessTarai :: Int -> Int -> Int -> Int
taraiLessTarai x y z
    | x <= y    = y
    | y <= z    = z
    | otherwise = x
    
tarai' :: Int -> Int -> Int -> Int
tarai' x y z
    | x <= y = y
    | otherwise = let z' = tarai' (z - 1) x y
                  in z' `seq` tarai'
                              (tarai' (x - 1) y z)
                              (tarai' (y - 1) z x)
                              z'
