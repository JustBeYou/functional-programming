import Data.Maybe

--- Limbajul si  Interpretorul

newtype IntState a = IntState { runIntState :: Integer -> (a, Integer) }

instance Show a => Show (IntState a) where
    show ma = show $ runIntState ma 0

instance Monad IntState  where
    return x = IntState $ \s -> (x, s) 
    ma >>= k = IntState f
        where 
            f state = 
                let (x, s) = runIntState ma state
                    in runIntState (k x) s

modify :: (Integer -> Integer) -> IntState ()
modify f = IntState $ \s -> ((), f s) 

tick :: IntState ()
tick = modify (+1)

get :: IntState a -> Integer
get (IntState a) =  

instance Applicative IntState where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return $ f a

instance Functor IntState where
    fmap f m = pure f <*> m

type M a = IntState a

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Out Term
  deriving (Show)

pgm :: Term
pgm = App
  (Lam "y"
    (App
      (App
        (Lam "f"
          (Lam "y"
            (App (Var "f") (Var "y"))
          )
        )
        (Lam "x"
          (Var "x" :+: Var "y")
        )
      )
      (Con 3)
    )
  )
  (Con 4)


data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

interp :: Term -> Environment -> M Value
interp (Var name) env = return $ fromMaybe Wrong (lookup name env) 
interp (Con x) _ = return $ Num x
interp (a :+: b) env = do
    aVal <- interp a env
    bVal <- interp b env
    add aVal bVal
interp (Lam name body) env = return $ Fun (\v -> interp body ((name, v) : env))
interp (App fun arg) env = do
    argValue <- interp arg env
    funBody <- interp fun env
    apply funBody argValue

add :: Value -> Value -> M Value
add (Num a) (Num b) = return $ Num $ a+b
add _ _ = return Wrong

apply :: Value -> Value -> M Value
apply (Fun f) v = f v
apply _ _ = return Wrong

showM :: Show a => M a -> String
showM ma = case (runStringWriter ma) of
    (a, s) -> s

test :: Term -> String
test t = showM $ interp t []

pgm1:: Term
pgm1 = Out $ App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))

main = do
    print $ test pgm1
    print $ showM $ interp (Var "x") []
