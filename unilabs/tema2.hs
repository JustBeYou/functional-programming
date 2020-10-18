import Data.Char 

-- 2.4
pozitiiImpare :: [Integer] -> [Integer]
pozitiiImpare [] = []
pozitiiImpare x = pozitiiImpareRec x 0

pozitiiImpareRec :: [Integer] -> Integer -> [Integer]
pozitiiImpareRec [] _ = []
pozitiiImpareRec (x:xs) pozitie
    | x `mod` 2 == 1 = pozitie : urmatorul
    | otherwise = urmatorul
    where urmatorul = pozitiiImpareRec xs (pozitie+1)

pozitiiImpareComp :: [Integer] -> [Integer]
pozitiiImpareComp lista = [pozitie | 
    (valoare, pozitie) <- (zip lista [0..]),
    valoare `mod` 2 == 1] 

-- 2.5
multDigitRec :: String -> Int
multDigitRec [] = 1
multDigitRec (x:xs)
    | isDigit x = (digitToInt x) * urmatorul
    | otherwise = urmatorul
    where urmatorul = multDigitRec xs

multDigitComp :: String -> Int
multDigitComp x = foldl (*) 1 [digitToInt y | y <- x, isDigit y]

-- 2.6
discount :: [Double] -> Double -> Double -> [Double]
discount prices percentage treshold = 
    [newPrice | newPrice <- map (* (1 - (percentage/100))) prices, newPrice < treshold]

main = do 
    print $ pozitiiImpare [0,1,(-3),(-2),8,(-1),6,1]
    print $ pozitiiImpareComp [0,1,(-3),(-2),8,(-1),6,1]
    print $ multDigitRec "Test 1234"
    print $ multDigitComp "Test 1234"
    print $ discount [150, 300, 250, 200, 450, 100] 25 200

