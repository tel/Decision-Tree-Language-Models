{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as F
import Data.Ord (comparing)
import Data.List
import Text.ParserCombinators.Parsec
import Data.Monoid
import Control.Arrow

import qualified ClusterTree as C
import BTree
import Text
import Agglomeration

tree1 :: BTree Char
tree1 = fmap (head . Set.toList) $ read "((  (e (u (o (i a))))) (h ((n (l (x r))) ((y (s (g d))) (((t k) (w c)) ((p (b (q j))) (f (m (z v)))))))))"

bsenc :: Map.Map Char Path
bsenc = bsEncode tree1

bitPredicts
  :: Ord k => Map.Map k Path -> [(k, k, k, t)] -> Maybe [(t, [Path])]
bitPredicts bs = mapM fn
  where fn (a, b, c, d) = do Path aenc <- Map.lookup a bs
                             Path benc <- Map.lookup b bs
                             Path cenc <- Map.lookup c bs
                             return $ (d, [Path cenc, Path benc, Path aenc])

Just dev4g = bitPredicts bsenc $ fourgram dev
Just cross4g = bitPredicts bsenc $ fourgram cross

-- Find the best possible splitting question given a set of data (for
-- the given node) and a key which states what the currently available
-- question positions in the bitstream are.
bestSplit :: [(Char, [Path])] -- Data set to split against
             -> [Int]      -- Keys indicating which questions to consider
             -> (Int, Float)
bestSplit dat key = maximumBy (comparing snd) options
  where options = map (snd &&& splitEntropy dat) (zip [0..] key)
                
splitEntropy :: [(Char, [Path])] -> (Int, Int) -> Float
splitEntropy dat = entropy2 . splitByCoordinate
  where entropy2 (left, right) = (nl/n) * (entropy left) + (nr/n) * (entropy right)
          where nl = fromIntegral $ length left
                nr = fromIntegral $ length right
                n  = nl + nr
        splitByCoordinate (i, j) = (map fst *** map fst) (partition pred dat)
          where pred :: (t, [Path]) -> Bool
                pred (_, hist) = unPath (hist !! i) !! j == TLeft

