import Text.Printf
import Data.List
import Control.Monad

type Nume = String
data Prop = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:

--- Ex 1
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&:
        ((Not (Var "P") :|: Not (Var "Q")) :&: 
         (Not (Var "P") :|: Not (Var "R")))

--- Ex 2
instance Show Prop where
    -- base case
    show (Var nume) = nume
    show T = "1"
    show F = "0"
    -- nested nots
    show (Not (left :|: right)) = "~(" ++ (show (left :|: right)) ++ ")"
    show (Not (left :&: right)) = "~(" ++ (show (left :&: right)) ++ ")"
    -- simple not
    show (Not prop) = "~" ++ (show prop)
    -- nested and/or
    show ((left1 :|: right1) :&: (left2 :|: right2)) = "(" ++ (show (left1 :|: right1)) ++ ")" ++ "&"
                                                       ++ "(" ++ (show (left2 :|: right2)) ++ ")" 
    show ((left1 :|: right1) :&: right) = "(" ++ (show (left1 :|: right1)) ++ ")" ++ "&" ++ (show right)
    show (left :&: (left2 :|: right2)) = (show left) ++ "&" ++ "(" ++ (show (left2 :|: right2)) ++ ")"
    -- simple and/or
    show (left :|: right) = (show left) ++ "|" ++ (show right)
    show (left :&: right) = (show left) ++ "&" ++ (show right)

--- Ex 3
type Env = [(Nume,Bool)]
data Result e v = Error e | Value v
type EvalResult = Result String Bool

instance (PrintfArg e, Show v) => Show (Result e v) where
    show (Error e) = printf "Errors found:\n%s" e
    show (Value v) = "Result: " ++ (show v)

eval :: Prop -> Env -> EvalResult
eval T _ = Value True
eval F _ = Value False
eval (Var name) env = safeLookup name env
eval (Not prop) env = evalUnary not (eval prop env) 
eval (left :|: right) env = evalBinary (||) (eval left env) (eval right env) 
eval (left :&: right) env = evalBinary (&&) (eval left env) (eval right env)

evalUnary :: (Bool -> Bool) -> EvalResult -> EvalResult
evalUnary func (Error e) = Error e 
evalUnary func (Value b) = Value $ func b

evalBinary :: (Bool -> Bool -> Bool) -> EvalResult -> EvalResult -> EvalResult
evalBinary func (Value left) (Value right) = Value $ func left right
evalBinary func (Value _) (Error right) = Error right
evalBinary func (Error left) (Value _) = Error left
evalBinary func (Error left) (Error right) = Error $ left ++ right

safeLookup :: (Eq a, Show a) => a -> [(a,b)] -> Result String b
safeLookup key dict = case (lookup key dict) of
    Nothing -> Error ((show key) ++ " not found.\n")
    (Just val)  -> Value val

--- Ex 4
variabile :: Prop -> [Nume]
variabile (Var nume) = [nume]
variabile (left :&: right) = nub $ (variabile left) ++ (variabile right)
variabile (left :|: right) = nub $ (variabile left) ++ (variabile right)
variabile (Not prop) = nub $ variabile prop
variabile _ = []

-- Ex 5
envs :: [Nume] -> [Env]
envs vars = map (\v -> zip vars v) (replicateM (length vars) [True, False]) 

-- Ex 6
satisfiabila :: Prop -> Bool
satisfiabila prop = or $ map (\env -> valoareAdev prop env) (envs $ variabile prop)

valoareAdev :: Prop -> Env -> Bool
valoareAdev prop env = case (eval prop env) of
    (Value v) -> v
    _ -> False

-- Ex 7
valida :: Prop -> Bool
valida prop = satisfiabila (Not prop) == False

main = do
    print p1
    print p2
    print p3
    print (Not(Var"P"):&:Var"Q")
    print $ eval (Var"L":|:Var"K") [("P",True), ("Q",False)]
    print $ eval (Var"P":|:Var"Q") [("P",True), ("Q",False)]
    print $ variabile ((Var"P":|:Var"Q"):&:(Var"K":|:Var"Q"))
    print $ envs $ variabile ((Var"P":|:Var"Q"):&:(Var"K":|:Var"Q"))
    print $ satisfiabila (Not(Var"P"):&:Var"Q")==True
    print $ satisfiabila (Not(Var"P"):&:Var"P")==False
    print $ valida (Not(Var"P"):&:Var"Q")==False
    print $ valida (Not(Var"P"):|:Var"P")==True
