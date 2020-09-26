import Debug.Trace

--- Given two infinite lists (sorted), calculate their difference in O(N)
-- THE INPUT MUST BE SORTED!

--- This function does not work on infinite lists as length x forces the evaluation
minus :: (Eq a, Ord a, Show a) => [a] -> [a] -> [a]
minus x y
  | length y == 0 = x
  | length x == 0 = []
  | head x < head y =  head x : minus (tail x) y
  | head x == head y = minus (tail x) y
  | otherwise = minus x (tail y) 

minusi :: (Eq a, Ord a, Show a) => [a] -> [a] -> [a]
--- On the other hand, pattern matching is the right choice
minusi x [] = x
minusi [] y = []
minusi (x:xs) (y:ys) 
  | x < y = x : minusi xs (y:ys)
  | x == y = minusi xs (y:ys)
  | otherwise = minusi (x:xs) ys

numbers = minus [1,2,3,4,5] [1,2,3]
numbersi = minusi [1,2..] [1,3..]

main = do
  print "Testing"
  print (take 5 numbers)
  print (take 5 numbersi)
