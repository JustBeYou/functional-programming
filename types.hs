ignoreReturn :: a -> (b -> c) -> (b -> c)
ignoreReturn r f = f

f :: Integral a => a -> Maybe a
f x
    | even x = Just (div x 2)
    | otherwise = Nothing

main = do
    print $ f 5
    print $ f 6
    print $ (>>=) (Just 5) f
    print $ (>>=) (Just 6) f
    let x = (Just 5) >>= f
    let y = (Just 6) >>= f
    print x
    print y
    print $ ignoreReturn (print "evaluated") (odd) 5
