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
    _ == _ = False

instance (Ord a) => Ord (Arbore a) where
    Vid <= _ = True
    _ <= Vid = True
    (Frunza x)  <= (Frunza y)   = x <= y
    (Nod _ x _) <= (Frunza y)   = x <= y
    (Frunza x)  <= (Nod _ y _)  = x <= y
    (Nod _ x _) <= (Nod _ y _)  = x <= y

esteArboreCautare :: (Eq a, Ord a, Bounded a) => Arbore a -> Bool
esteArboreCautare Vid = True
esteArboreCautare (Frunza _) = True
esteArboreCautare (Nod stanga x dreapta) = and [
    esteArboreCautareUtil stanga minBound x,
    esteArboreCautareUtil dreapta x maxBound]

esteArboreCautareUtil :: (Eq a, Ord a, Bounded a) => Arbore a -> a -> a -> Bool
esteArboreCautareUtil Vid _ _ = True
esteArboreCautareUtil (Frunza x) min max = x > min && x < max 
esteArboreCautareUtil (Nod stanga x dreapta) min max = and [
    esteArboreCautareUtil stanga min x,
    esteArboreCautareUtil dreapta x max]

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

    -- invalid
    let arb1 = Nod (Nod Vid 2 (Frunza 5)) 4 Vid :: Arbore Int
    print arb1 
    print $ esteArboreCautare arb1

    -- valid
    let arb2 = Nod (Frunza 3) 4 (Frunza 5) :: Arbore Int
    print arb2
    print $ esteArboreCautare arb2

    -- invalid (cu dublura)
    let arb3 = Nod (Frunza 3) 4 (Nod (Frunza 5) 5 Vid) :: Arbore Int
    print arb3
    print $ esteArboreCautare arb3

    -- invalid (cu dublura)
    let arb4 = Nod (Frunza 3) 4 (Nod Vid 5 (Frunza 5)) :: Arbore Int
    print arb4
    print $ esteArboreCautare arb4

