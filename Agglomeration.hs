module Agglomeration where

import qualified Data.Set as Set
import qualified Data.Foldable as F
import Data.Ord (comparing)
import Data.List

import BTree
import Text

-- mutual information under frequency `f` with clustering imposed by
-- the forest `trees`. This is pretty damn slow, mostly in the
-- cluster_bf and cluster_uf list comprehensions
--
-- TODO: Route a "simplifying" cluster bigram frequency through the
-- recursion so that freqOf runs more and more quickly
mi :: (F.Foldable t, Ord o) =>
      Frequency o 
      -> Frequency (o, o) 
      -> [t (Set.Set o)] 
      -> Float
mi uf bf trees = sum $ filter (not . isNaN) 
                 $ {-# SCC "info_prod" #-} 
                 [info c1 c2 | c1 <- clusters, c2 <- clusters, c1 /= c2]
  where clusters = map (F.foldMap id) trees
        cluster_bf c1 c2 = {-# SCC "cl_bf" #-} 
          sum [ freqOf (a, b) bf | a <- Set.toList c1, b <- Set.toList c2 ]
        cluster_uf c1 = {-# SCC "cl_uf" #-} 
          sum [ freqOf a uf | a <- Set.toList c1 ] 
        info c1 c2 = let p = cluster_bf c1 c2
                     in  {-# SCC "info_comp" #-} 
                      p * (log $ p / (cluster_uf c1 * cluster_uf c2))
uf = unigram_f train
bf = bigram_f train                        
                     
agglom :: Ord o => 
          Frequency o
          -> Frequency (o, o)
          -> [BTree (Set.Set o)]
          -> BTree o
agglom uf bf trees = fmap (head . Set.toList) $ head $ step trees                     
  where step (t:[]) = [t]
        step ts = step $ snd 
                  $ maximumBy (comparing fst)
                  $ map (\x -> (mi uf bf x, x))
                  $ [ (Split a b) : (delete b $ delete a ts)
                    | a <- ts, b <- ts
                    , a /= b ]


