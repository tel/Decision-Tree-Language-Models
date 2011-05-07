module Agglomeration
    ( unfoldClusterTree,
      mutualInfo, 
      BSTree(..), bsSingleton ) where

import qualified Data.Set as S
import Data.List (nub, delete, minimumBy, maximumBy)
import Data.Ord (comparing)
import qualified Data.Foldable as F (sum)
import Control.Monad (replicateM)
import Control.Monad.State
import Control.Arrow
import GHC.Exts (the)

import Stats
import DTree

-- some types for agglomeration
type BSTree a = BTree (S.Set a)
type FreqSt a = (Freq (a, a), Freq a)
bsSingleton = Leaf . S.singleton

-- iteratively cluster a group of atoms such that at each step the
-- merged atoms (sets of atoms) are the ones with maximal mutual
-- information
-- unfoldClusterTree :: Ord a => Freq (a, a) -> [a] -> BTree a
unfoldClusterTree freq2g vocab = 
    fmap (head . S.elems) $ 
    the $ snd $ execState (replicateM (n-1) stepClustering) ((freq2g, freq1g), atoms)
    where n = length vocab
          freq1g = marginalize fst freq2g
          atoms  = map bsSingleton (nub vocab)

-- perform a single cluster update step
stepClustering :: (Ord a, Eq a) => State (FreqSt a, [BSTree a]) ()
stepClustering = do
  ((bg, ug), trees) <- get
  let n = length trees
      merges = do i <- [0..(n-1)]
                  j <- [(i+1)..(n-1)]
                  let a = trees !! i
                      b = trees !! j
                      m = bBranch a b
                      t' = delete a (delete b trees)
                  return ((a, b), m : t')
      (bestPair, bestSet) = maximumBy (comparing $ mutualInfo bg ug . snd) merges
  put ((bg, ug), bestSet)


-- Compute the mutual information over groupings based on the bigram
-- and unigram class frequencies
mutualInfo
  :: Ord a => Freq (a, a) -> Freq a -> [DTree s (S.Set a)] -> Double
mutualInfo bg ug trees = sum $ filter (not . isNaN) $ map mi is
    where n  = length trees
          is = [(i, j) | i <- [0..(n-1)], j <- [0..(n-1)]]
          mi (i, j) =  mi' 
                       (flatten $ fmap S.toList $ trees !! i) 
                       (flatten $ fmap S.toList $ trees !! j)
          mi' ci cj = let num = sum $ [freqOf (ai, aj) bg | ai <- ci, aj <- cj]
                          denom = (F.sum $ fmap (\a -> freqOf a ug) ci) * 
                                  (F.sum $ fmap (\a -> freqOf a ug) cj)
                      in num * log2 (num/denom)