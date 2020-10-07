bad :: Integer -> [Integer]
bad x = [i+j+2*i*j| i<-[1..x], j<-[1..x], i+j+2*i*j <= x]

primes :: [Integer]
primes = [2] ++ [2*x+1 | x<-[1..], not (elem x (bad x))]
