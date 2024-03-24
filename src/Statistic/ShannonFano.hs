{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : ???
-}
module Statistic.ShannonFano (tree) where

import Statistic.EncodingTree
import Statistic.Source (orderedCounts) -- on utilise orderedCounts pour obtenir les comptes triés des symboles
import Data.List (sortBy) -- Fonction de tri
import Data.Function (on) -- Fonction pour composer des fonctions

-- | Construit l'arbre de Shannon-Fano pour une liste de symboles
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree symbols
  | null symbols = Nothing -- Si la liste de symboles est vide on retourne Nothing (rien)
  | otherwise = Just $ buildTree $ sortBy (flip compare `on` snd) $ orderedCounts symbols
  -- Sinon, retourne Just de l'arbre construit à partir des symboles triés par ordre décroissant

-- | Construit l'arbre de codage à partir d'une liste triée de symboles + fréquences
buildTree :: [(a, Int)] -> EncodingTree a
buildTree [] = error "Cannot build tree from empty list" -- Erreur si la liste est vide
buildTree [(sym, cnt)] = EncodingLeaf cnt sym -- Si list n'a qu'un seul element alors on crée une feuille de l'arbre
buildTree symbols = EncodingNode total leftTree rightTree
  where
    total = sum $ map snd symbols -- Calcule la somme totale des fréquences
    (left, right) = splitList symbols -- Divise la liste en deux 
    leftTree = buildTree left -- Construit l'arbre pour la partie gauche avec recurisiite
    rightTree = buildTree right -- Construit l'arbre pour la partie droite avec recursivite

-- | Divise une liste en deux sous-listes aussi équilibrées que possible en termes de fréquence totale
splitList :: [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitList symbols = splitAt (findSplitIndex symbols) symbols -- Divise la liste à l'indice retourné par findSplitIndex

-- | Trouve l'index où diviser la liste pour obtenir deux sous-listes équilibrées
findSplitIndex :: [(a, Int)] -> Int
findSplitIndex symbols = go 0 0 (sum $ map snd symbols) symbols -- Démarre la recherche de l'indice à partir de 0
  where
    go idx leftSum total (x:xs)
      | leftSum * 2 >= total = idx -- Si la somme des fréquences à gauche est au moins la moitié du total alors on retourne l'indice actuel
      | otherwise = go (idx + 1) (leftSum + snd x) total xs --  Sinon on continue la recherche avec l'indice incrémenté et la somme mise à jour
    go _ _ _ [] = 0 -- Si la liste est vide alors retourne 0