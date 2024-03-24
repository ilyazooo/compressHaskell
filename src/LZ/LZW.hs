{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : Adel YOUSSOUF ALI
-}
module LZ.LZW(compress, uncompress) where

import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map

-- | LZW compress method
compress :: String -> [Int]
compress input = snd $ foldl process (initialDict, []) input
  where
    -- Dictionnaire initial avec tous les caractères possibles
    initialDict = Map.fromList $ zip (map (:[]) ['\0'..'\65000']) [0..]
    -- Processus de compression pour chaque caractère
    process (dict, output) nextChar =
      let newStr = currentStr ++ [nextChar]
      in case Map.lookup newStr dict of
           -- Si la chaîne est déjà dans le dictionnaire
           Just _ -> (dict, output ++ [fromJust $ Map.lookup currentStr dict])
           -- Si la chaîne n'est pas dans le dictionnaire
           Nothing -> (Map.insert newStr (Map.size dict) dict, output ++ [fromJust $ Map.lookup currentStr dict])
      where
        -- Chaîne courante pour la recherche dans le dictionnaire
        currentStr = [nextChar]

-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress codes = uncompress' codes 65001 (Map.fromList $ zip [0..65000] (map (:[]) ['\0'..'\65000'])) ""
  where
    uncompress' [] _ _ acc = Just acc
    uncompress' (c:cs) nextCode reverseDict acc =
      case Map.lookup c reverseDict of
        -- Si le code n'est pas dans le dictionnaire
        Nothing -> Nothing
        Just str ->
          let newAcc = acc ++ str
              -- Création d'une nouvelle entrée pour le dictionnaire inversé
              newEntry = (nextCode, head str : take 1 (fromMaybe "" $ Map.lookup (head cs) reverseDict))
              -- Mise à jour du dictionnaire inversé avec la nouvelle entrée
              newReverseDict = if null cs then reverseDict else uncurry Map.insert newEntry reverseDict
          in uncompress' cs (nextCode + 1) newReverseDict newAcc
