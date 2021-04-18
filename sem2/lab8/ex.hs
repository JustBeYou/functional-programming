module Main where

newtype Parser a = Parser { apply :: String->[(a,String)] }

parse :: Parser a -> String -> a
parse m s = head [x | (x,t) <- apply m s, t == ""]

instance Monad Parser where
    return a = Parser $ \s -> [(a, s)]
    ma >>= k = Parser $ \s -> [(b, s'') | (a, s') <- apply ma s, (b, s'') <- apply (k a) s']

instance Applicative Parser where
    pure = return
    mf <*> ma = do { f <- mf; f <$> ma; }

instance Functor Parser where
    fmap f ma = f <$> ma

empty :: Parser a
empty = Parser $ const []

anychar :: Parser Char
anychar = Parser f where
    f [] = []
    f (c:s) = [(c,s)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy prop = do
    c <- anychar
    if prop c then return c
    else empty

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = empty
string [c] = do 
    c' <- char c
    return [c']
string s = do 
    c <- char $ head s
    s' <- string $ tail s
    return (c:s')

three :: Parser (Char, Char)
three = do
    a <- anychar
    b <- anychar
    c <- anychar
    return (a,c)

s = "12345"
main = do
    print "lab 8"
    print $ apply anychar s
    print $ apply (satisfy (== 'c')) s
    print $ apply (satisfy (== 'c')) ""
    print $ apply (satisfy (== '1')) s
    print $ apply (string "123") s
    print $ apply (string "12") s
    print $ apply (string "1") s
    print $ apply (string []) s
    print $ apply three "12345"
    print $ apply three "12"