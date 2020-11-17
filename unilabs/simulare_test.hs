import Data.Char
import Debug.Trace
import Test.QuickCheck

f :: Char -> Bool
f c 
    | isUpper c = c < 'M'
    | isLower c = c < 'm'
    | otherwise = error "Nu este litera"

g :: String -> Bool
g s = (sum [1 | c <- sa, f c]) > (sum [1 | c <- sa, not (f c)])
    where sa = onlyAlpha s

onlyAlpha :: String -> String
onlyAlpha s = [c | c <- s, isAlpha c]

h :: String -> Bool
h s = rh (onlyAlpha s) 0 0

rh :: String -> Int -> Int -> Bool
rh [] x y = x > y
rh (s:ss) x y
    | f s = rh ss (x+1) y
    | otherwise = rh ss x (y+1)

prop_gh s = (g s) == (h s)

c :: [Int] -> [Int]
c [] = []
c (x:[]) = []
c (x:y:xs) 
    | x == y = x : (c (y:xs))
    | otherwise = c (y:xs)

d :: [Int] -> [Int]
d xs = [x | (i,x) <- zip [0..] xs, i + 1 < length xs, x == xs!!(i+1)]

prop_cd xs = (c xs) == (d xs)

main = do
    print $ f 'e'
    print $ h "\170587"
    print $ g "SyzYGy"
    print $ h "SyzYGy"
    print $ c []
    print $ c [1]
    print $ c [3,1,1,3,3,5]
    print $ c [4,1,1,1,4,4]
    print $ d [3,1,1,3,3,5]
    print $ d [4,1,1,1,4,4]
    quickCheck prop_cd
    quickCheck prop_gh
