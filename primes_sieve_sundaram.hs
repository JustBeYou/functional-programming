--- Calculate infinite list of primes using the Sieve of Sundaram

--- Given two sorted infinite lists x and y, calculate the difference x \ y
minusi :: (Eq a, Ord a, Show a) => [a] -> [a] -> [a]
minusi x [] = x
minusi [] y = []
minusi (x:xs) (y:ys) 
  | x < y = x : minusi xs (y:ys)
  | x == y = minusi xs (y:ys)
  | otherwise = minusi (x:xs) ys

--- bad numbers in the sieve: i + j + 2 * i * j, where 1 <= i <= j
bad = [i + j + 2 * i * j | 
  i <- [1..], 
  j <- [1..],
  1 <= i && i <= j
  ]

primify :: Integer -> Integer
primify x = 2 * x + 1

primes = 2 : map primify (minusi [1..] bad)

main = do
  print (take 20 primes)
--- [2,3,5,7,11,13,17,19,23,25,29,31,35,37,41,43,47,49,53,55]
