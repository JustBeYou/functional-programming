import Test.QuickCheck

{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\ x -> g x >>= f)

asoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
asoc f g h x = undefined 

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

foo :: Maybe Int ->  Maybe Bool 
foo  mx =  mx  >>= (\x -> Just (pos x))  
 
fooDo :: Maybe Int -> Maybe Bool
fooDo mx = do
    x <- mx
    return $ pos x

fooProp :: Maybe Int -> Bool
fooProp x = (fooDo x) == (foo x) 

addM :: Maybe Int -> Maybe Int -> Maybe Int  
addM mx my = undefined

fTest :: Int -> Maybe Int
fTest x = (pure . (2*)) <=< (pure . (3+)) $ x

main = do
    print "Monade"
    print $ fTest 3
