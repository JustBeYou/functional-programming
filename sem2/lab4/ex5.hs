import Data.Maybe

--- Limbajul si  Interpretorul

newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }

instance Show a => Show (StringWriter a) where
    show ma = show $ runStringWriter ma

tell :: String -> StringWriter ()
tell s = StringWriter((), s)

instance Monad StringWriter where
    return x = StringWriter (x, "")
    ma >>= k = do
        let (a, sa) = runStringWriter ma
        let (b, sb) = runStringWriter $ k a
        StringWriter (b, sa ++ sb) 

instance Applicative StringWriter where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return $ f a

instance Functor StringWriter where
    fmap f m = pure f <*> m

type M a = StringWriter a

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
interp (Out x) env = do
    val <- interp x env
    tell $ (show val) ++ "; "
    return val
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
