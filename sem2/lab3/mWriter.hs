import Control.Monad

newtype WriterS a = Writer { runWriter :: (a, String) } deriving Show 

instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)

instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = tell ("Increment " ++ (show x) ++ "\n") >> return (x+1)

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n 
    | n <= 0 = return x
    | otherwise = logIncrement x >> logIncrementN (x+1) (n-1)

main = do
    print "test"
    print $ runWriter $ logIncrement 2
    print $ runWriter $ logIncrementN 2 4
