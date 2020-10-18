data Alegere = Piatra | Foarfeca | Hartie
               deriving (Eq, Show)

data Rezultat = Victorie | Infrangere |Egalitate
               deriving (Eq, Show)

partida :: Alegere -> Alegere -> Rezultat
partida Piatra Foarfeca = Victorie
partida Hartie Piatra   = Victorie
partida Foarfeca Hartie = Victorie
partida x y | x == y = Egalitate
            | otherwise = Infrangere

main = do
    print $ partida Piatra Foarfeca
    print $ partida Foarfeca Foarfeca
    print $ partida Foarfeca Piatra
