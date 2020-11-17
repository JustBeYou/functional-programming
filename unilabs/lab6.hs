import Data.Foldable
import Data.Functor
import Test.QuickCheck

data Linie a =   L [a]
data Matrice a = M [Linie a]

instance (Show a) => Show (Linie a) where
    show (L []) = ""
    show (L (x:xs)) = (show x) ++ " " ++ (show (L xs))

instance (Show a) => Show (Matrice a) where
    show (M []) = ""
    show (M (x:[])) = (show x)
    show (M (x:xs)) = (show x) ++ "\n" ++ (show (M xs))

instance Foldable Linie where
    foldMap f (L []) = mempty
    foldMap f (L (x:xs)) = mappend (f x) (foldMap f xs)

instance (Arbitrary a) => Arbitrary (Linie a) where
    arbitrary = do
            p <- vector 100
            return $ L p

prop_linieSum :: (Num a, Eq a) => Linie a -> Bool
prop_linieSum (L xs) = (sum xs) == (sum $ L xs) 

sumN :: Int -> Matrice Int -> Bool
sumN _ (M []) = False
sumN n (M xs) = foldr (&&) True (map ((== n) . sum) xs) 

doarPozN :: Int -> Matrice Int -> Bool
doarPozN _ (M []) = False
doarPozN n (M xs) = foldr (&&) True (map isValid xs)
    where isValid (L ys) = if (length ys) == n then (all (>0) ys) else True

main = do
    print $ (foldr (+) 0 (L [1, 2, 3]))
    print $ (sum (L [1, 2, 3])) 
    print $ (prop_linieSum (L [1, 2, 3]))
    quickCheck (prop_linieSum :: Linie Int -> Bool)
    print $ sumN 6 (M [L [1,2,3], L [3,2,1]])
    print $ sumN 6 (M [L [1,2,3], L [3,3,1]])
    print $ M [L [1, 2, 3], L [3, 2, 1], L [0, 0, 0]]
    print $ length (L [1,2,3])
    print $ doarPozN 3 (M[L[1,2,3],L[4,5],L[2,3,6,8],L[8,5,3]])
    print $ doarPozN 3 (M[L[1,2,-3],L[4,5],L[2,3,6,8],L[8,5,3]])
