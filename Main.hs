module Main where


-- Alright.
import qualified Data.Set as S
import Data.List (nub, delete, maximumBy)
import Data.Ord (comparing)
import Control.Monad.State
import Control.Arrow

import Text
import Stats
import DTree

-- some types for agglomeration
type BSTree a = BTree (S.Set a)
type FreqSt a = (Freq (S.Set a, S.Set a), Freq (S.Set a))

-- iteratively cluster a group of atoms such that at each step the
-- merged atoms (sets of atoms) are the ones with maximal mutual
-- information
-- unfoldClusterTree :: Ord a => Freq (a, a) -> [a] -> BTree a
unfoldClusterTree freq vocab = atoms
    where freq1g = marginalize (S.singleton . snd) freq
          freq2g = imap (S.singleton *** S.singleton) freq
          atoms  = map (Leaf . S.singleton) (nub vocab)

-- perform a single cluster update step
stepClustering :: (Ord a, Eq a) =>
                  [BSTree a] 
               -> State (FreqSt a) [BSTree a]
stepClustering trees = do
  (bg, ug) <- get
  let merges = [((a, b), (Branch () a b) : (delete a (delete b trees)))
                | a <- trees, b <- trees]
      best   = fst $ maximumBy (comparing snd) $
               zip merges (map (mutualInfo bg ug . snd) merges)
  return (snd best)


-- Compute the mutual information over groupings based on the bigram
-- and unigram class frequencies
mutualInfo :: Ord a
           => Freq (S.Set a, S.Set a) -- bigram freq
           -> Freq (S.Set a)          -- unigram freq
           -> [BTree (S.Set a)]       -- current clusters
           -> Double                  -- mutual information
mutualInfo bg ug trees = sum $ filter (not . isNaN) $ map mi is
    where n  = length trees
          is = [(i, j) | i <- [0..(n-1)], j <- [(i+1)..(n-1)]]
          mi (i, j) =  mi' (flatten $ trees !! i) (flatten $ trees !! j)
          mi' ci cj = let num = freqOf (ci, cj) bg
                          denom = (freqOf ci ug) * (freqOf cj ug)
                      in num * log2 (num/denom)
                      


main = do train <- readFile "textA.txt"
          let train2g = bigrams train
              bg      = imap (S.singleton *** S.singleton) $ freqFrom train2g
              ug      = imap S.singleton $ freqFrom train
              alpha   = "abcdefghijklmnopqrstuvwxyz "
              a0      = map (Leaf . S.singleton) alpha
          print $ evalState (stepClustering a0) (bg, ug)