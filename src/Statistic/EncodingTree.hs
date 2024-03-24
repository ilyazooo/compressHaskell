{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : ???
-}

module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress) where
import Statistic.Bit
import Data.List(nub)

-- Définition du type de l'arbre d'encodage
data EncodingTree a = EncodingNode Int (EncodingTree a) (EncodingTree a) | EncodingLeaf Int a deriving (Eq, Show)

-- | Is the encoding a mere leaf ?
isLeaf :: EncodingTree a -> Bool
isLeaf (EncodingLeaf _ _) = True  -- Retourne vrai si c'est une feuille
isLeaf _ = False  -- Retourne faux sinon

-- | The length of the underlying source
count :: EncodingTree a -> Int
count (EncodingLeaf cnt _) = cnt  -- Retourne le compteur pour une feuille
count (EncodingNode cnt _ _) = cnt  -- Retourne le compteur pour un nœud

-- | Search for symbol in encoding tree
has :: Eq a => EncodingTree a -> a -> Bool
has (EncodingLeaf _ x) y = x == y  -- Vérifie si le symbole correspond
has (EncodingNode _ l r) y = has l y || has r y  -- Vérifie récursivement dans les sous-arbres

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode t x = go t x []
  where
    go (EncodingLeaf _ y) z acc
      | y == z = Just acc  -- Si le symbole correspond, retourne l'accumulateur
      | otherwise = Nothing  -- Sinon, retourne Nothing
    go (EncodingNode _ l r) z acc
      | has l z = go l z (acc ++ [Zero])  -- Si le symbole est dans le sous-arbre gauche, ajoute Zero et continue
      | has r z = go r z (acc ++ [One])  -- Si le symbole est dans le sous-arbre droit, ajoute One et continue
      | otherwise = Nothing  -- Si le symbole n'est pas trouvé, retourne Nothing

-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce _ [] = Nothing  -- Retourne Nothing si la liste de bits est vide
decodeOnce t bits = go t bits
  where
    go (EncodingLeaf _ x) remaining = Just (x, remaining)  -- Retourne le symbole et les bits restants
    go (EncodingNode _ l r) (Zero:bs) = go l bs  -- Si le bit est Zero, continue avec le sous-arbre gauche
    go (EncodingNode _ l r) (One:bs) = go r bs  -- Si le bit est One, continue avec le sous-arbre droit
    go _ _ = Nothing  -- Retourne Nothing si aucun cas ne correspond

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode t bits = go t bits []
  where
    go _ [] acc = Just (reverse acc)  -- Si la liste de bits est vide, retourne les symboles accumulés
    go t bs acc = case decodeOnce t bs of
      Just (x, remaining) -> go t remaining (x:acc)  -- Si un symbole est décodé, continue avec les bits restants
      Nothing -> Nothing  -- Retourne Nothing si le décodage échoue

-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength t = fromIntegral (go t 0) / fromIntegral (count t)
  where
    go :: EncodingTree a -> Int -> Int
    go (EncodingLeaf _ _) n = n  -- Pour une feuille, retourne la profondeur actuelle
    go (EncodingNode _ l r) n = go l (n + 1) + go r (n + 1)  -- Pour un nœud, calcule la somme des profondeurs des sous-arbres

-- | Compress method using a function generating encoding tree and also returns generated encoding tree
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress f xs
  | length (nub xs) == 1 = (Just (EncodingLeaf (length xs) (head xs)), replicate (length xs) Zero)
  -- Si tous les symboles sont identiques, retourne une feuille et une séquence de Zeros
  | otherwise = case f xs of
      Just t -> (Just t, concatMap (maybe [] id . encode t) xs)
      -- Si l'arbre est généré, encode la séquence et retourne l'arbre et la séquence encodée
      Nothing -> (Nothing, [])
      -- Si l'arbre n'est pas généré, retourne Nothing

-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Just (EncodingLeaf _ x), bits) = Just (replicate (length bits) x)
-- Si l'arbre est une simple feuille, réplique le symbole pour chaque bit
uncompress (Just t, bits) = decode t bits  -- Utilise l'arbre pour décoder la séquence de bits
uncompress (Nothing, _) = Nothing  -- Retourne Nothing si l'arbre n'est pas fourni