{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : Adel YOUSSOUF ALI
-}
module LZ.LZ78 (compress, uncompress) where

import qualified LZ.Dictionaries as D
import Data.Maybe (mapMaybe)
import Data.List (elemIndex)

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress input = compress' input [] -- Appel initial à la fonction compress' avec l'entrée et un dictionnaire vide
  where
    -- Fonction récursive qui comprime l'entrée en une liste de tuples (indice, caractère)
    compress' :: String -> D.Dictionary -> [(Int, Char)]
    compress' [] _ = [] -- Cas de base : lorsque l'entrée est vide, renvoie une liste vide
    compress' input dict =
      let (prefix, rest) = findLongestPrefix input dict -- Trouve le plus long préfixe de l'entrée dans le dictionnaire
          (nextChar, remainingRest) = case rest of
                                        [] -> ('\0', []) -- Si rest est vide, affecte '\0' à nextChar et une liste vide à remainingRest
                                        (x:xs) -> (x, xs) -- Sinon, affecte le premier caractère de rest à nextChar et le reste à remainingRest
          newDict = dict ++ [prefix ++ [nextChar]] -- Ajoute le préfixe plus le prochain caractère à la fin du dictionnaire
      in (findIndex prefix dict, nextChar) : compress' remainingRest newDict -- Appelle récursivement compress' avec le reste de l'entrée et le nouveau dictionnaire

findIndex :: String -> D.Dictionary -> Int
findIndex prefix dict = case elemIndex prefix dict of
                          Nothing -> 0 -- Si le préfixe n'est pas trouvé dans le dictionnaire, renvoie 0
                          Just index -> index + 1 -- Sinon, renvoie l'indice plus un

findLongestPrefix :: String -> D.Dictionary -> (String, String)
findLongestPrefix input dict = go input "" -- Appel initial à la fonction go avec l'entrée et une chaîne vide comme préfixe
  where
    go :: String -> String -> (String, String)
    go [] acc = (acc, []) -- Cas de base : lorsque l'entrée est vide, renvoie le préfixe accumulé et une liste vide
    go (x:xs) acc =
      if elem (acc ++ [x]) dict -- Vérifie si le préfixe accumulé suivi du caractère actuel existe dans le dictionnaire
        then go xs (acc ++ [x]) -- Si oui, continue à accumuler le préfixe
        else (acc, x:xs) -- Sinon, renvoie le préfixe accumulé et le reste de l'entrée

-- | LZ78 uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(Int, Char)] -> Maybe String
uncompress [] = Just "" -- Si l'entrée est vide, renvoie une chaîne vide emballée dans Just
uncompress input = fmap concat $ sequence $ snd $ foldl process (D.empty, []) input where
  process (dict, output) (idx, c) =
    if idx > length dict then (dict, output ++ [Nothing]) -- Si l'indice est supérieur à la taille du dictionnaire, ajoute Nothing à la sortie
    else let entry = if idx == 0 then [c] else if c=='\0' then dict !! (idx) else dict !! (idx) ++ [c]  
             newDict = dict ++ [entry]
         in (newDict, output ++ [Just entry]) -- Ajoute Just entry à la sortie et met à jour le dictionnaire