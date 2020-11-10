semn:: [Integer] -> String
semn [] = []
semn (x:xs)
    | x >= -9 && x <= 9 = semnNumar x ++ semn xs
    | otherwise = semn xs

semnNumar x 
    | x == 0 = "0"
    | x < 0  = "-"
    | x > 0  = "+"

main = do
    print $ semn [5, 10, -5, 0]
