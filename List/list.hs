mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + length xs


