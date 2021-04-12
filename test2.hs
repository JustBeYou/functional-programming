import Test.QuickCheck

myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 xs ys zs = map zipHelper (zip (zip xs ys) zs)

zipHelper :: ((a,b),c) -> (a,b,c)
zipHelper ((x,y),z) = (x,y,z)

myzip3NoAux :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3NoAux xs ys zs = map (\((x,y),z) -> (x,y,z)) (zip (zip xs ys) zs)

prop_myzip3 :: [Int] -> [Int] -> [Int] -> Bool
prop_myzip3 x y z = (myzip3 x y z) == (zip3 x y z)

prop_myzip3NoAux :: [Int] -> [Int] -> [Int] -> Bool
prop_myzip3NoAux x y z = (myzip3NoAux x y z) == (zip3 x y z)

myzip3mz :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3mz a b c =
  if length a < length b && length a < length c
    then
      if length b < length c
        then map (\(y, x, ind) -> (c !! ind, y, x)) $ map (\(x, ind) -> (b !! ind, x, ind)) (zip a [0 ..])
        else map (\(y, x, ind) -> (b !! ind, y, x)) $ map (\(x, ind) -> (c !! ind, x, ind)) (zip a [0 ..])
    else
      if length b < length c
        then
          if length a < length c
            then map (\(y, x, ind) -> (c !! ind, y, x)) $ map (\(x, ind) -> (a !! ind, x, ind)) (zip b [0 ..])
            else map (\(y, x, ind) -> (a !! ind, y, x)) $ map (\(x, ind) -> (c !! ind, x, ind)) (zip b [0 ..])
        else
          if length a < length b
            then map (\(y, x, ind) -> (b !! ind, y, x)) $ map (\(x, ind) -> (a !! ind, x, ind)) (zip c [0 ..])
            else map (\(y, x, ind) -> (a !! ind, y, x)) $ map (\(x, ind) -> (b !! ind, x, ind)) (zip c [0 ..])

prop_myzip3mz :: [Int] -> [Int] -> [Int] -> Bool
prop_myzip3mz x y z = (myzip3mz x y z) == (zip3 x y z)

main = do
    quickCheck prop_myzip3
    quickCheck prop_myzip3NoAux
    print $ myzip3mz [0] [0] [1,0]
    quickCheck prop_myzip3mz
