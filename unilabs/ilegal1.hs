import Data.Char
import Data.List

type Ingredient = String
type Quantity = Int
data Recipe =  R (Ingredient, Quantity) | Recipe  :::  Recipe
    deriving Show
infixr 5 :::

lowerRecipe :: Recipe -> Recipe
lowerRecipe (R (i, q)) = R (map toLower i, q)
lowerRecipe (x ::: y) = (lowerRecipe x) ::: (lowerRecipe y)

toList :: Recipe -> [(Ingredient, Quantity)]
toList (R (i, q)) = [(i, q)]
toList (x ::: y) = toList x ++ toList y

sumup :: [(Ingredient, Quantity)] -> Ingredient -> (Ingredient, Quantity)
sumup xs i = (i, sum [q | (ii, q) <- xs, ii == i])

uniques :: [(Ingredient, Quantity)] -> [(Ingredient, Quantity)]
uniques xs = [sumup xs i | i <- nub [ii | (ii, q) <- xs]]

fromList :: [(Ingredient, Quantity)] -> Recipe
fromList ((i, q):[]) = R (i,q)
fromList ((i, q):xs) = R (i,q) ::: fromList xs

formatRecipe :: Recipe -> Recipe
formatRecipe = fromList . uniques . toList . lowerRecipe

instance Eq Recipe where
    x == y = (sort $ (uniques . toList . lowerRecipe) x) == (sort $ (uniques . toList . lowerRecipe) y)

r1 = R ("fAIna", 500) ::: R ( "oua" , 4) ::: R ("zahar", 500)
r2 = R ("faina", 500) ::: R ("oua",2) ::: R ( "zahar", 500) :::  R ("Oua", 2)
r3 = R ("faina", 500) ::: R ("zahar", 500) ::: R ( "Oua", 55)

main = do
    print $ lowerRecipe (R ("faina", 500) ::: R ("oua",2) ::: R ( "zahar", 500) :::  R ("Oua", 2)) 
    let l = toList $ lowerRecipe (R ("faina", 500) ::: R ("oua",2) ::: R ( "zahar", 500) :::  R ("Oua", 2))
    print $ nub [i | (i, q) <- l]
    print $ sumup l "oua"
    print $ formatRecipe (R ("faina", 500) ::: R ("oua",2) ::: R ( "zahar", 500) :::  R ("Oua", 2))
    print $ r1 == r2
    print $ r1 == r3 
    print "fac afaceri ilegale"

