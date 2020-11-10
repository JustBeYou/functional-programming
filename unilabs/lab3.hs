
numaiVocale :: [String] -> [String]
numaiVocale l = map numaiVocaleCuvant l

numaiVocaleCuvant :: String -> String
numaiVocaleCuvant s = filter esteVocala s

esteVocala :: Char -> Bool
esteVocala c = elem c "aeiouAEIOU" 

main = do
    print $ esteVocala 'a'
    print $ numaiVocaleCuvant "salut"
    print $ numaiVocale ["salut", "Ana", "Maria"]
