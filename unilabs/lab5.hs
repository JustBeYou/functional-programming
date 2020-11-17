import Test.QuickCheck

semn :: [Integer] -> String
semn [] = ""
semn (x:xs) = (semn_cifra x) ++ (semn xs)

semn_cifra :: Integer -> String
semn_cifra x
    | elem x [(-9)..(-1)] = "-" 
    | elem x [1..9]   = "+"
    | x == 0          = "0"
    | otherwise       = ""

semnFold :: [Integer] -> String
semnFold = foldr op unit
    where
        unit = ""
        x `op` result = (semn_cifra x) ++ result

prop_semn :: [Integer] -> Bool
prop_semn xs = (semn xs) == (semnFold xs)

corect :: [[a]] -> Bool
corect [] = True
corect xs = foldr (&&) True (map f xs)
    where f y = (length y) == (length (xs!!0))

el :: [[a]] -> Int -> Int -> a
el m x y = (m!!x)!!y

transforma :: [[a]] -> [[(a, Int, Int)]]
transforma m = [[(vy, x, y) | (y, vy) <- zip [1..] vx] | (x, vx) <- zip [1..] m] 

main = do
    print $ "lab 5"
    print $ semn [5,10,-5,0]
    quickCheck prop_semn
    print $ corect [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    print $ el [[1, 2, 3], [4, 5, 6], [7, 8, 9]] 1 2
    print $ transforma [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

