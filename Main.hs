{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import qualified Data.Foldable as F
import Data.Monoid

import Text

data BTree a = Split (BTree a) (BTree a)
             | Leaf a
             | Empty
               deriving Eq
                        
instance Show (BTree (Set.Set Char)) where
  show (Leaf x) = (Set.toList x)
  show (Split l r) = "(" ++ show l ++ " " ++ show r ++ ")"

instance F.Foldable BTree where                        
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Split y z) = F.foldMap f y `mappend` F.foldMap f z
               
-- Get our alphabet
alphabet = "abcdefghijklmnopqrstuvwxyz "
alpha1 = alphabet -- "abcdfgefghijklmno"
singletons = map (Leaf . Set.singleton) alphabet

-- Agglomerate the forest into a single tree
-- agglom :: Map.Map Bigram Freq -> [BTree (Set.Set Bigram)] -> BTree (Set.Set Bigram)
-- agglom _ (tree:[]) = tree
-- agglom f trees = head trees
--   where f' = Map.mapKeys Set.singleton f
  
-- agglom_step :: (Map.Map (Set.Set Bigram) Freq, [BTree (Set.Set Bigram)]) ->
--                (Map.Map (Set.Set Bigram) Freq, [BTree (Set.Set Bigram)])
-- agglom_step (f, tree:[]) = (f, [tree])
-- agglom_step (f, trees) = (f', trees')
--   where f' = Map.insert comb comb

-- mutual information under frequency `f` with clustering imposed by
-- the forest `trees`. This is pretty damn slow, mostly in the
-- cluster_bf and cluster_uf list comprehensions
--
-- TODO: Route a "simplifying" cluster bigram frequency through the
-- recursion so that freqOf runs more and more quickly
mi :: (F.Foldable t, Ord o) =>
      Frequency o -> Frequency (o, o) -> [t (Set.Set o)] -> Float
mi uf bf trees = sum $ filter (not . isNaN) 
                 $ {-# SCC "info_prod" #-} [info c1 c2 | c1 <- clusters, c2 <- clusters, c1 /= c2]
  where clusters = map (F.foldMap id) trees
        cluster_bf c1 c2 = {-# SCC "cl_bf" #-} sum [ freqOf (a, b) bf 
                               | a <- Set.toList c1
                               , b <- Set.toList c2 ]
        cluster_uf c1 = {-# SCC "cl_uf" #-} sum [ freqOf a uf 
                            | a <- Set.toList c1 ] 
        info c1 c2 = let p = cluster_bf c1 c2
                     in  {-# SCC "info_comp" #-} p * (log $ p / (cluster_uf c1 * cluster_uf c2))
uf = unigram_f train
bf = bigram_f train                        
                     
agglom
  :: Ord o =>
     Frequency o
     -> Frequency (o, o)
     -> [BTree (Set.Set o)]
     -> BTree (Set.Set o)
agglom uf bf trees = head $ step trees                     
  where step (t:[]) = [t]
        step ts = step $ snd 
                  $ maximumBy (\a b -> compare (fst a) (fst b))
                  $ map (\x -> (mi uf bf x, x))
                  $ [ (Split a b) : (delete b $ delete a ts)
                    | a <- ts, b <- ts
                    , a /= b ]
                  
tree = agglom uf bf $ map (Leaf . Set.singleton) alpha1

main = print tree