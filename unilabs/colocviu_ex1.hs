import Test.QuickCheck

--- Toate vocalele dintr-un sir sunt litere mari este echivalent cu
--- Sirul nu contine nicio vocala litera mica
vocale_mici = "aeiou"

funaFM :: String -> Bool
funaFM [] = True
funaFM (x:xs)
    | elem x vocale_mici = False
    | otherwise = funaFM xs

funbFM :: String -> Bool
funbFM xs
    | length [x | x <- xs, elem x vocale_mici] > 0 = False
    | otherwise = True

prop_abFM :: String -> Bool
prop_abFM xs = (funaFM xs) == (funbFM xs)

funcFM :: String -> Bool
funcFM xs = length (filter (\x -> elem x vocale_mici) xs) == 0

prop_bcFM :: String -> Bool
prop_bcFM xs = (funbFM xs) == (funcFM xs)

main = do
    print $ funaFM "abcdef"
    print $ funaFM "1231AEIOU"
    print $ funaFM "AEIoU"
    quickCheck prop_abFM
    quickCheck prop_bcFM
