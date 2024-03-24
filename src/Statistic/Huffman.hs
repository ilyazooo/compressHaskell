{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : ???
-}

module Statistic.Huffman(tree) where
import Statistic.EncodingTree
import Statistic.Source (orderedCounts)
import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree symbols
  | null counts = Nothing  -- Si la liste des comptes est vide, retourne Nothing
  | otherwise = Just $ buildTree (map (\(s, c) -> EncodingLeaf c s) sorted)
  -- Sinon, construit l'arbre en utilisant la fonction buildTree sur les feuilles encodées triées
  where
    counts = orderedCounts symbols  -- Compte et ordonne les symboles
    sorted = sortBy (comparing snd) counts  -- Trie les comptes par leur fréquence

    -- Fonction interne pour construire l'arbre de Huffman
    buildTree :: [EncodingTree a] -> EncodingTree a
    buildTree [t] = t  -- Si un seul arbre est présent, c'est l'arbre final
    buildTree (t1:t2:ts) =
      -- Combine les deux arbres avec les plus petites fréquences
      let combined = EncodingNode (count t1 + count t2) t1 t2
      -- Crée un nouveau nœud avec ces deux arbres et la somme de leurs fréquences
      in buildTree $ insertBy (comparing count) combined ts
      -- Insère le nouvel arbre combiné dans la liste des arbres de manière ordonnée et continue le processus de construction