import Data.Maybe

--- Limbajul si  Interpretorul

newtype IntState a = IntState { runIntState :: Integer -> (a, Integer) }

instance (Show a) => Show (IntState a) where
    show x = show $ runIntState x 0

instance Monad IntState where
    return x = IntState $ \s -> (x, s)
    ma >>= k = IntState f where
                f s = runIntState mb s' where
                    (a, s') = runIntState ma s
                    mb = k a

instance Applicative IntState where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return $ f a

instance Functor IntState where
    fmap f ma = pure f <*> ma

modify :: (Integer -> Integer) -> IntState ()
modify f = IntState $ \s -> ((), f s)

tickS :: IntState ()
tickS = modify $ \s -> s + 1

get :: IntState Integer 
get = IntState $ \s -> (s, s)

type M a = IntState a

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Count
  deriving (Show)


data Value = Num Integer
           | Fun (Value -> M Value) 
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"

type Environment = [(Name, Value)]

interp :: Term -> Environment -> M Value
interp (Var name) env = lookupM name env
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
interp Count env = do
    x <- get
    return $ Num x

lookupM :: Name -> Environment -> M Value
lookupM name env = case lookup name env of
    Just x -> return x
    Nothing -> return Wrong

add :: Value -> Value -> M Value
add (Num a) (Num b) = do 
    tickS
    return $ Num $ a+b
add _ _ = do
    tickS
    return Wrong

apply :: Value -> Value -> M Value
apply (Fun f) v = do
    tickS
    f v
apply _ _ = do
    tickS
    return Wrong

test :: Term -> String
test t = show $ interp t []

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

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+: (Con 11))

pgm2:: Term
pgm2 = ((Con 1 :+: Con 2) :+: Count)          

main = do
    print $ test pgm
    print $ test pgm1
    print $ test pgm2
