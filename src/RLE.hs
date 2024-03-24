{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}
module RLE(compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = compress' xs x 1 -- | x est le premier element et xs le reste de la liste
    where
        -- | Cas où la liste restante est vide
        compress' [] currentRun runLength = [(currentRun, runLength)]-- | On renvoie le tuple contenant le caractère etudié et son nombre d'apparition consecutive
        -- | Cas où la liste restante n'est pas vide
        compress' (y:ys) currentRun runLength
            | y == currentRun = compress' ys currentRun (runLength + 1) -- | Si le premier element de notre liste est le meme que le caractere etudié On incrémente le nombre d'apparition consecutive et on rappelle la fontion compress' sur la nouvelle liste restante
            | otherwise = (currentRun, runLength) : compress' ys y 1 -- | Sinon on renvoie le tuple contenant le caractère etudié et son nombre d'apparition consecutive et on rappelle la fonction compress' sur la nouvelle liste restante avec un nombre d'apparition consecutif de 1
            
-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Nothing
uncompress xs = Just $ concatMap (\(x, n) -> replicate n x) xs
-- | On utilise la fonction concatMap pour mapper chaque tuple (x, n) dans la liste d'entrée xs à une liste de longueur n où chaque élément est x