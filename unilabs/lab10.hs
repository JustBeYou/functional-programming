import Test.QuickCheck
import Test.QuickCheck.Gen

-- Exr 1
semigroupAssoc_prop :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc_prop a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity_prop :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity_prop a = (mempty <> a) == a

monoidRightIdentity_prop :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity_prop a = (a <> mempty) == a

data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivId = Trivial -> Bool

-- Exr 2
newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a && b)

instance Monoid BoolConj where
    mempty = BoolConj True

instance Arbitrary BoolConj where
    arbitrary = MkGen (\s i -> BoolConj ((unGen arbitrary) s i))

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjId    = BoolConj -> Bool

-- Ex 6
data Or a b = Fst a | Snd b
    deriving (Eq, Show)

instance Semigroup (Or a b) where
    Fst _ <> x = x
    y     <> _ = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = oneof [
       MkGen (\s i -> Fst ((unGen arbitrary) s i)),
       MkGen (\s i -> Snd ((unGen arbitrary) s i))]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool
                    
main = do
    print "Lab 10"
    quickCheck (semigroupAssoc_prop :: TrivAssoc)
    quickCheck (monoidLeftIdentity_prop :: TrivId)
    quickCheck (monoidRightIdentity_prop :: TrivId)
    quickCheck (semigroupAssoc_prop :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity_prop :: BoolConjId)
    quickCheck (monoidRightIdentity_prop :: BoolConjId)
    quickCheck (semigroupAssoc_prop :: OrAssoc String Int)
    quickCheck (semigroupAssoc_prop :: OrAssoc String [Int])

