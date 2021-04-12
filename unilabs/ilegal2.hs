data E x = A | M (E x) x Int deriving Eq

instance Foldable E where
    foldMap f A = mempty
    foldMap f (M x y z) = (foldMap f x) `mappend` (f y)

fTest0 = maximum (M (M (M (M A "nota" 2) "zece" 3) "la" 5) "examen" 1) == "zece"

class C e where
    cFilter :: Monoid a => (a->Bool) -> e a -> e (Maybe a)
    fromList :: (Monoid a, Eq a) => [a] -> e a

instance C E where
    cFilter f A = if (f mempty) then M A (Just mempty) 0 else M A Nothing 0
    cFilter f (M x y z) = if (f y) then (M x (Just y) z) else (M x Nothing z)

main = do
    print fTest0
    print "tra la la"
