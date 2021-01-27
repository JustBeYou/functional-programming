import Test.QuickCheck

funFM :: [Int] -> [Int]
funFM xs = fun_auxFM xs False 0

fun_auxFM :: [Int] -> Bool -> Int -> [Int]
fun_auxFM [] _ _ = []
fun_auxFM (x:xs) in_secv lung
    | este_cifraFM x && in_secv = lung : (fun_auxFM xs True 0)
    | este_cifraFM x            = fun_auxFM xs True 0
    | in_secv                   = fun_auxFM xs True (lung+1)
    | otherwise                 = fun_auxFM xs False 0

este_cifraFM :: Int -> Bool
este_cifraFM x = (x >= -9) && (x <= 9)  

main = do
    print $ funFM [1, 11, 12, 13, 14, 15, 2, 21, 22,3, 5, 51, 52,2, 21] 
    print $ funFM [11, 12, 13, 14, 15, 2, 21, 22,3, 5, 51, 52,2, 21]
    print $ funFM [1, 11, 12, 13, 14, 15, 2, 21, 22,3, 5, 51, 52,2, 21, 1]
    print $ funFM []
    print "Colocviu Sem I"
