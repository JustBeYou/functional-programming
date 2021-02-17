--- Ex 1
data Prog = On Instr
data Instr = Off | Expr :> Instr
data Expr = Mem | V Int | Expr :+ Expr
type Env = Int
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int

prog :: Prog -> DomProg
prog (On s) = stmt s 0

stmt :: Instr -> DomInstr
stmt (ex :> is) env = let val = expr ex env in (val : (stmt is val))
stmt Off env = []

expr :: Expr -> DomExpr
expr Mem env = env
expr (V i) _ = i
expr (ex1 :+ ex2) env = (expr ex1 env) + (expr ex2 env)

--- Ex 2
type Name = String
data Hask = HTrue
 | HFalse
 | HLit Int
 | HLet Name Hask Hask
 | HIf Hask Hask Hask
 | Hask :==: Hask
 | Hask :+: Hask
 | HVar Name
 | HLam Name Hask
 | Hask :$: Hask
  deriving (Read, Show)
infix 4 :==:
infixl 6 :+:
infixl 9 :$:

data Value = VBool Bool
 | VInt Int
 | VFun (Value -> Value)
 | VError -- pentru reprezentarea erorilor
type HEnv = [(Name, Value)]

instance Show Value where
    show (VBool b) = show b
    show (VInt i) = show i
    show (VFun _) = "Functie Value -> Value"
    show VError = "Eroare"

instance Eq Value where
    (VBool a) == (VBool b) = a == b
    (VInt  a) == (VInt  b) = a == b
    a         == b         = error ((show b) ++ " nu se poate compara cu " ++ (show a))

type DomHask = HEnv -> Value

hEval :: Hask -> DomHask
-- literals
hEval HTrue  _   = VBool True
hEval HFalse _   = VBool False
hEval (HLit i) _ = VInt i

-- variables
hEval (HVar name) env = case (lookup name env) of
    Nothing -> error $ "Variabila " ++ name ++ " nu este definita!"
    Just v  -> v
hEval (HLam name ex) env = VFun (\value -> hEval ex ((name, value):env))
hEval (HLet name val ex) env = hEval ex ((name, (hEval val env)):env)

-- operations
hEval (expr1 :==: expr2) env = VBool $ (hEval expr1 env) == (hEval expr2 env)
hEval (expr1 :+:  expr2) env = case ((hEval expr1 env), (hEval expr2 env)) of
    (VInt a, VInt b) -> VInt $ a + b
    (a, b) -> error $ "Nu se poate efectua " ++ (show a) ++ " + " ++ (show b) 

-- statements
hEval (HIf cond thenBranch elseBranch) env = case (hEval cond env) of
    (VBool True) -> hEval thenBranch env
    (VBool False) -> hEval elseBranch env
    v -> error $ "Expresia nu are valoare logica: " ++ (show v)
hEval (expr1 :$: expr2) env = case ((hEval expr1 env),(hEval expr2 env)) of
    ((VFun f), v) -> f v
    (nonf, _) -> error $ (show nonf) ++ " nu este o functie!"

main = do
    print $ prog (On ((V 3) :> ((Mem :+ (V 5)) :> Off)))
    print (VInt 5)
    print $ (VInt 5) == (VInt 4)
    --print $ (VInt 5) == (VBool True)

    print $ hEval (HLet "x" (HLit 5) (HLet "y" (HLit 5) ((HVar "x") :+: (HVar "y")))) []
