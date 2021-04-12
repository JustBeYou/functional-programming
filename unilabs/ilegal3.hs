data U a = U { getU :: [a] }
    deriving (Show, Eq)

instance Foldable U where
    foldMap f x = foldMap f (getU x)

main = do
    print "123"
