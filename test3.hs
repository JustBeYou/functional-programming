main = do
    print "hello"
    print $ [1, 2, 4]

f :: Fractional b => [b] -> [b]
f xs = map (/5) xs
