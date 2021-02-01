data Expr = X             -- variabila
       | Const Int     -- constanta de tip întreg
       | Neg Expr      -- minus unar
       | Expr :+: Expr -- adunare
       | Expr :*: Expr -- multiplicare
       deriving Show

-- punctul A
fppFM :: Expr -> [String]
fppFM X = ["X"]
fppFM (Const x) = [show x]
fppFM (Neg expr)  = (fppFM expr) ++ ["-"]
fppFM (expr1 :+: expr2) = (fppFM expr1) ++ (fppFM expr2) ++ ["+"]
fppFM (expr1 :*: expr2) = (fppFM expr1) ++ (fppFM expr2) ++ ["*"]

-- punctul B
-- Nu am tratat cazul in care input-ul e invalid
-- Nu am tratat posibile erori, am considerat input-ul ca fiind corect
fromFppFM :: [String] -> Expr
fromFppFM idents = fromFppUtilFM idents []

-- Reconstruiesc expresia folosind o stiva
-- Rezultatul va fi in capul stivei
fromFppUtilFM :: [String] -> [Expr] -> Expr
fromFppUtilFM [] stack = stack!!0
fromFppUtilFM ("-":idents) (fst:stack) = fromFppUtilFM idents ((Neg fst):stack)
fromFppUtilFM ("+":idents) (fst:snd:stack) = fromFppUtilFM idents ((snd :+: fst):stack) 
fromFppUtilFM ("*":idents) (fst:snd:stack) = fromFppUtilFM idents ((snd :*: fst):stack)
fromFppUtilFM ("X":idents) stack = fromFppUtilFM idents (X:stack)
fromFppUtilFM (int:idents) stack = fromFppUtilFM idents ((Const (read int)):stack) 

-- Testare
main = do
    print "Examen 1"
    print $ fppFM ((Const 5 :+: Neg X) :*: Const 17)
    print $ fromFppFM ["5", "X", "-", "+", "17", "*"]
    print $ fromFppFM ["15", "7", "X", "1", "+", "*", "-", "+", "3", "*"]

