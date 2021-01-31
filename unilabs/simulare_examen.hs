import Text.Printf

data Arbore a = Nod (Arbore a) a (Arbore a)
    | Frunza a
    | Vid

instance (Eq a) => Eq (Arbore a) where
    Vid == Vid = True  
    (Frunza x)  == (Frunza y)   = x == y
    (Nod _ x _) == (Frunza y)   = x == y
    (Frunza x)  == (Nod _ y _)  = x == y
    (Nod _ x _) == (Nod _ y _)  = x == y

instance (Ord a) => Ord (Arbore a) where
    Vid <= _ = True
    _ <= Vid = True
    (Frunza x)  <= (Frunza y)   = x <= y
    (Nod _ x _) <= (Frunza y)   = x <= y
    (Frunza x)  <= (Nod _ y _)  = x <= y
    (Nod _ x _) <= (Nod _ y _)  = x <= y

esteArboreCautare :: (Eq a, Ord a) => Arbore a -> Bool
esteArboreCautare Vid = True
esteArboreCautare (Frunza _) = True
esteArboreCautare (Nod stanga x dreapta) = and [stanga < (Frunza x),
                                                (Frunza x) < dreapta,
                                                esteArboreCautare stanga,
                                                esteArboreCautare dreapta]

insereaza :: (Eq a, Ord a) => a -> Arbore a -> Arbore a
insereaza x Vid = Frunza x
insereaza x (Frunza y) 
    | x == y = Frunza x
    | x <  y = Nod (Frunza x) y Vid
    | x >  y = Nod Vid y (Frunza x)
insereaza x (Nod stanga y dreapta) 
    | x == y = Nod stanga x dreapta
    | x <  y = Nod (insereaza x stanga) y dreapta
    | x >  y = Nod stanga y (insereaza x dreapta)

instance Functor Arbore where
    fmap _ Vid = Vid
    fmap f (Frunza x) = Frunza (f x)
    fmap f (Nod stanga x dreapta) = Nod (fmap f stanga) (f x) (fmap f dreapta)

instance Foldable Arbore where
    foldMap f Vid = mempty
    foldMap f (Frunza x) = f x
    foldMap f (Nod stanga x dreapta) = (foldMap f stanga) `mappend` (f x) `mappend` (foldMap f dreapta)

instance Show a => Show (Arbore a) where
    show Vid = ""
    show (Frunza x) = show x
    show (Nod Vid x Vid) = show x
    show (Nod stanga x Vid) = printf "%s, %s" (show stanga) (show x)
    show (Nod Vid x dreapta) = printf "%s, %s" (show x) (show dreapta)
    show (Nod stanga x dreapta) = printf "%s, %s, %s" (show stanga) (show x) (show dreapta)

main = do
    print "Simulare examen"
    let arb = insereaza 5 (insereaza 3 (insereaza 12 (insereaza 7 (insereaza 6 (insereaza (-1) (insereaza 8 (insereaza 100 (insereaza (-25) Vid))))))))
    print arb
    print $ fmap (+25) arb
    print $ foldr (+) 0 arb
