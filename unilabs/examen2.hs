data B e = e :|: e 
         | B e ::: B e
infixr 5 :::
infixr 6 :|:

--- Punctul C
instance Foldable B where
    foldMap f (x :|: y) = (f x) `mappend` (f y)
    foldMap f (x ::: y) = (foldMap f x) `mappend` (foldMap f y)

fTest0FM = maximum ("Mama" :|: "are" ::: "patru" :|: "mere" ::: "" :|: "si" ::: "doua" :|: "pere") == "si"

class C e where
    cFilterFM :: Monoid a => (a -> Bool) -> e a -> e a
    toListFM :: (Monoid a, Eq a) => e a -> [a]

--- Punctul D
instance C B where
    cFilterFM f (x :|: y) = ((if (f x) then x else mempty) :|: (if (f y) then y else mempty))
    cFilterFM f (x ::: y) = ((cFilterFM f x) ::: (cFilterFM f y))
    
    toListFM (x :|: y) = filter (\x -> x /= mempty) [x, y]
    toListFM (x ::: y) = (toListFM x) ++ (toListFM y)

cTest0FM =
    toListFM (cFilterFM (\x -> length x > 3)
        ("Mama" :|: "are" ::: "patru" :|: "mere" ::: "" :|: "si" ::: "doua" :|: "pere")) ==
     ["Mama", "patru", "mere", "doua", "pere"]

--- Testare
main = do
    print "Examen 2"
    print fTest0FM
    print cTest0FM
