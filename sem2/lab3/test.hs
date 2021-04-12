import Control.Monad

myGetLine :: IO String
myGetLine = do
    x <- getChar
    if x == '\n' then do
        return []
    else do
        xs <- myGetLine
        return (x:xs)

main = do
    print "Salut"
    line <- myGetLine
    print line
