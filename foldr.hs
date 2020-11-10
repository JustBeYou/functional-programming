sp :: [Integer] -> Integer
sp [] = 0
sp (a : as)
    | odd a = a * a + sp as
    | otherwise = sp as

sumaPatrateImpare :: [Integer] -> Integer
sumaPatrateImpare = foldr op 0
    where x `op` total = patratImpar x + total 

patratImpar :: Integer -> Integer
patratImpar x = if odd x then x^2 else 0 

main = do
    print $ sp [1,2,3,4,5]
    print $ sumaPatrateImpare [1,2,3,4,5]
