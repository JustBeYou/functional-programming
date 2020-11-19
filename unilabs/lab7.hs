import Test.QuickCheck
import Data.Char

double :: Int -> Int
double x = 2 * x
triple :: Int -> Int
triple x = 3 * x
penta :: Int -> Int
penta x = 5 * x

test x = (double x + triple x) == (penta x)
test2 x = (double x + triple x) /= (double x + triple (x+1))

myLookUp :: Int -> [(Int,String)]-> Maybe String
myLookUp _ [] = Nothing
myLookUp n ((i,v):xs)
    | n == i = Just v
    | otherwise = myLookUp n xs

myLookUp' :: Int -> [(Int,String)] -> Maybe String
myLookUp' n xs = capitalize $ myLookUp n xs

capitalize :: Maybe String -> Maybe String
capitalize Nothing = Nothing
capitalize (Just "") = Just ""
capitalize (Just s) = Just ((toUpper $ head s) : (tail s))

testLookUp :: Int -> [(Int,String)] -> Bool
testLookUp n xs = (myLookUp n xs) == (lookup n xs)

testLookUp' :: Int -> [(Int,String)] -> Bool
testLookUp' n xs = (myLookUp' n xs) == (capitalize $ lookup n xs)

testLookUpCond :: Int -> [(Int,String)] -> Property
testLookUpCond n list = n > 0 && n `div` 5 == 0 ==> testLookUp n list

data ElemIS = I Int | S String
     deriving (Show,Eq)

instance Arbitrary ElemIS where
    arbitrary = oneof [arbitraryInt, arbitraryString]
        where arbitraryInt = do
                p <- arbitrary
                return $ I p
              arbitraryString = do
                p <- arbitrary
                return $ S p

myLookUpElem :: Int -> [(Int,ElemIS)]-> Maybe ElemIS
myLookUpElem _ [] = Nothing
myLookUpElem n ((i,v):xs)
    | n == i = Just v
    | otherwise = myLookUpElem n xs

dummy :: [(Int,ElemIS)] -> [(Int,ElemIS)]
dummy [] = []
dummy ((i,v):xs) = (i+1,v) : (dummy xs)

testLookUpElem :: Int -> [(Int,ElemIS)] -> Bool
testLookUpElem n xs = (myLookUpElem n xs) == (myLookUpElem (n+1) (dummy xs)) 

main = do
    quickCheck test
    quickCheck test2
    quickCheck testLookUp
    quickCheck testLookUp'
    quickCheck testLookUpCond
    quickCheck testLookUpElem
