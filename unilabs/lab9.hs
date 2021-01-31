-- Ex 2

import Prelude hiding (lookup)
import qualified Data.List as List

class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert :: Ord key => key -> value -> c key value -> c key value
    lookup :: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    toList :: c key value -> [(key, value)]
   
    keys :: c key value -> [key]
    keys x = [k | (k, v) <- toList x]

    values :: c key value -> [value]
    values x = [v | (k, v) <- toList x]

    fromList :: Ord key => [(key, value)] -> c key value
    fromList [] = empty
    fromList ((k,v):[]) = singleton k v
    fromList ((k,v):kvs) = insert k v (fromList kvs)

newtype PairList k v = PairList 
    { getPairList :: [(k, v)] }
    deriving Show

listLookup :: Eq k => k -> [(k, v)] -> Maybe v
listLookup _ [] = Nothing
listLookup sk ((ik,iv):kvs) = if ik == sk then Just iv
                                else listLookup sk kvs

instance Collection PairList  where
    empty = PairList { getPairList = [] }
    singleton k v = PairList { getPairList = [(k,v)] }
    insert k v c = PairList 
        { getPairList = (k,v) : (toList $ delete k c)}
    lookup k c = listLookup k (getPairList c)
    delete k c = PairList 
        { getPairList = [(ik, iv) | 
            (ik, iv) <- (getPairList c),
            ik /= k] }
    toList c = getPairList c

p :: PairList String Int 
p = PairList { getPairList = [] }

p1 = insert "b" 3 (insert "b" 2 (insert "a" 1 p))
p2 = lookup "b" p1
p3 = lookup "c" p1
p4 = delete "a" p1

data SearchTree key value
    = Empty 
    | Node
        (SearchTree key value)
        key
        value
        (SearchTree key value)
    deriving Show

instance Collection SearchTree where
    empty = Empty
    singleton k v = Node Empty k v Empty

    insert k v Empty = singleton k v
    insert newKey newVal (Node left key val right)
        | newKey == key = Node left key newVal right
        | newKey <  key = Node (insert newKey newVal left) key val right
        | newKey >  key = Node left key val (insert newKey newVal right)

    lookup k Empty = Nothing
    lookup searchKey (Node left key val right)
        | searchKey == key = Just val
        | searchKey <  key = lookup searchKey left
        | searchKey >  key = lookup searchKey right

    delete k c = Empty
    
    toList Empty = []
    toList (Node left k v right) = (toList left) ++ [(k,v)] ++ (toList right)

t :: SearchTree String Int 
t = Empty

t1 = insert "c" 99 (insert "d" 5 (insert "b" 3 (insert "b" 2 (insert "a" 1 t))))
t2 = lookup "b" t1
t3 = lookup "c" t1
t4 = delete "a" t1
main = do
    print p1
    print p2
    print p3
    print p4
    print t1
    print t2
    print t3
    print t4
    print $ toList t1
    print "Lab 9"
