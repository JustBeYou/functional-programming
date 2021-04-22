import Data.Maybe
import Data.List

type Name = String

data  Pgm  = Pgm [Name] Stmt
        deriving (Read, Show)

data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt| Name := AExp | While BExp Stmt
        deriving (Read, Show)

data AExp = Lit Integer | AExp :+: AExp | AExp :-: AExp | AExp :*: AExp | Var Name
        deriving (Read, Show)

data BExp = BTrue | BFalse | AExp :==: AExp | Not BExp
        deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 6 :-:
infixl 7 :*:

type Env = [(Name, Integer)]

aEval :: AExp -> Env -> Integer
aEval (Lit x) env = x
aEval (Var name) env = lookupM name env
aEval (a :+: b) env = (aEval a env) + (aEval b env)
aEval (a :-: b) env = (aEval a env) - (aEval b env) 
aEval (a :*: b) env = (aEval a env) * (aEval b env) 

lookupM :: Name -> Env -> Integer
lookupM name env = case lookup name env of
    Just x -> x
    Nothing -> error $ "Variabila " ++ name ++ " nu este definita."

bEval :: BExp -> Env -> Bool
bEval BTrue env = True
bEval BFalse env = False
bEval (a :==: b) env = (aEval a env) == (aEval b env)
bEval (Not a) env = not (bEval a env)

sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (st1 ::: st2) env = sEval st2 (sEval st1 env)
sEval (If b st1 st2) env =  if (bEval b env) then (sEval st1 env) else (sEval st2 env) 
sEval (x := e) env = (x,(aEval e env)) : (filter (\(k,v) -> k /= x) env)
sEval (While cond st) env = case bEval cond env of
    True -> sEval (While cond st) (sEval st env)
    False -> env

pEval :: Pgm -> IO Env
pEval (Pgm lvar st) = do 
    let initial = [(k, 0) | k <- lvar]
    print $ "Stare initiala " ++ (show initial)
    let result = sEval st initial
    print $ "Stare finala " ++ (show result)
    return result
                       
factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )
    
gaussStmt :: Stmt
gaussStmt =
    "n" := Lit 5 ::: "i" := Var "n" ::: "s" := Lit 0 :::
    While (Not (Var "i" :==: Lit 0)) 
    ( "s" := Var "s" :+: Var "i" :::
      "i" := Var "i" :-: Lit 1
    )

test1 = Pgm [] factStmt
test2 = Pgm [] gaussStmt

main = do
    pEval test1
    pEval test2
