module Main where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { apply :: String->[(a, String)] }

instance Monad Parser where
    return a = Parser $ \s -> [(a, s)]
    ma >>= k = Parser $ \s -> concat [apply (k a) s' | (a, s') <- apply ma s]

instance Applicative Parser where
    pure = return
    mf <*> ma = do { f <- mf; a <- ma; return $ f a }

instance Functor Parser where
    fmap f ma = pure f <*> ma

instance Alternative Parser where
    empty = Parser $ const  []
    ma <|> mb = Parser $ \s -> case apply ma s of
        [] -> apply mb s
        (x:_) -> [x]

anychar :: Parser Char
anychar = Parser f where
    f [] = []
    f (c:s) = [(c, s)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy prop = do
    c <- anychar
    if prop c then return c
    else empty

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = return ""
string s = mapM char s

anycharord :: Parser Int
anycharord = fmap ord anychar

repeatM :: Parser a -> Parser [a]
repeatM p = do 
          x <- p
          xs <- repeatM p <|> return []
          return ( x : xs )

howmany :: Char -> Parser Int
howmany c = fmap length (repeatM $ char c)

main :: IO ()
main = do
    let s = "12345"
    print $ apply (string "123") s
    print $ apply (string "12") s
    print $ apply (string "1") s
    print $ apply (string []) s
    print $ apply anycharord s
    print $ apply (char 'a' <|> char 'b') s
    print $ apply (char '1' <|> char 'b') s
    print $ apply (char 'a' <|> char '1') s
    print $ apply (char '1' <|> char '1') s
    print $ apply (howmany 'a') "aaab"
    print $ apply (howmany 'a') "aaaaab"
    print $ apply (howmany 'a') "b"