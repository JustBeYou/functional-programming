--- Calculate infinite list of primes using the Sieve of Sundarami
import Data.List
import Debug.Trace

main = do
    print (take 200 primes)

primes = 2 : (map primify (minusi [1..] bad))
primify n = 2 * n + 1 

bad :: [Integer] 
bad = badIntern initialState

data State = State {
    chunkIndex :: Integer,
    precalc :: [Integer]
}
initialState = State 1 [4]

type SundaramGenerator = (State -> [Integer])
badIntern :: State -> [Integer]
badIntern (State ci pc) = do 
    let shouldUpdate = True   
    let nextCi = if shouldUpdate then ci + 1 else ci
    let nextPc = if shouldUpdate then (sort (pc ++ sundaramOf nextCi)) else pc
    
    return (head nextPc) ++ (badIntern (State nextCi (tail nextPc)))

sundaramOf :: Integer -> [Integer]
sundaramOf j = [i + j + 2 * i * j | i <- [1..j]]

boundOf :: Integer -> Integer
boundOf j = 2 * j * (j + 1)

--- Given two sorted infinite lists x and y, calculate the difference x \ y
minusi :: (Eq a, Ord a, Show a) => [a] -> [a] -> [a]
minusi x [] = x
minusi [] y = []
minusi (x:xs) (y:ys) 
  | x < y = x : minusi xs (y:ys)
  | x == y = minusi xs (y:ys)
  | otherwise = minusi (x:xs) ys


