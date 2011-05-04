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
import Data.Array
import Control.Arrow

import qualified ClusterTree as C
import qualified BTree as BT
import Motion
import Text
import Agglomeration

-- Introduce the encoding indices
-- EncIx :: (History index, Bitstream index)
type EncIx = (Int, -- History element index
              Int) -- Within-element bitstream index
newtype Bitstream = Bitstream (Array EncIx Motion)

instance Show Bitstream where
  show (Bitstream arr) = concatMap show (elems arr)

type EncData = [(Char, Bitstream)]

bitPredicts :: Map.Map Char Path -> [(Char, Char, Char, Char)] -> Maybe EncData
bitPredicts bs = mapM fn
  where fn (a, b, c, d) = do Path aenc <- Map.lookup a bs
                             Path benc <- Map.lookup b bs
                             Path cenc <- Map.lookup c bs
                             let n = length aenc
                                 full = listArray ((0, 0), (3, n)) 
                                        (cenc ++ benc ++ aenc)
                             return $ (d, Bitstream full)

-- Question tree
--
-- This datatype represents a binary decision tree that encodes a
-- question of the form (Split ix l r) ~> "continute to the left
-- branch if predictor at ix is TLeft and visa versa".
--
-- Leaves contain boolean values which determine whether they can
-- continue growing.
data QTree = Split EncIx QTree QTree | Leaf Bool deriving (Show, Eq)
deadLeaf = Leaf False
liveLeaf = Leaf True


-- Splits the data into two sets based on whether each item's
-- predictor at EncIx is TLeft (fst) or TRight (snd)
splitDataWith :: EncIx -> EncData -> (EncData, EncData)
splitDataWith ix = partition fn
    where fn (_, Bitstream arr) = arr ! ix == TLeft


-- Computes a sample entropy passed to a given Leaf in a QTree
entropyAtLeaf :: QTree -> Path -> EncData -> Double
entropyAtLeaf (Leaf _) (Path []) dat = entropy $ map fst dat
entropyAtLeaf (Leaf _) (Path (m:_)) _ = error "Path does not lead to leaf."
entropyAtLeaf (Split ix l r) (Path (m:ms)) dat = 
    case m of
      TLeft  -> entropyAtLeaf l (Path ms) (fst $ splitDataWith ix dat)
      TRight -> entropyAtLeaf r (Path ms) (snd $ splitDataWith ix dat)

                          
-- Splits a leaf adding the particular EncIx constraint to the tree                       
splitLeaf :: QTree -> Path -> EncIx -> QTree
splitLeaf (Leaf True) (Path []) ix = Split ix liveLeaf liveLeaf
splitLeaf (Leaf False) (Path []) ix = error "Cannot split a dead leaf."
splitLeaf (Leaf _) (Path (m:_)) _ = error "Path does not lead to leaf."
splitLeaf (Split ixhere l r) (Path (m:ms)) ix = 
    case m of 
      TLeft -> Split ixhere (splitLeaf l (Path ms) ix) r
      TRight -> Split ixhere l (splitLeaf r (Path ms) ix)

        
-- finds the best possible split of EncData given a bitmask of
-- previously asked questions
bestSplit :: EncData -> [Int] -> (EncIx, Double)
bestSplit dat mask = maximumBy (comparing snd) splits
    where splits :: [(EncIx, Double)]
          splits = map (id &&& (\ix -> splitEntropy (splitDataWith ix dat))) indices
          indices = zip [0..] mask
          splitEntropy :: (EncData, EncData) -> Double
          splitEntropy (as, bs) = let na = fromIntegral $ length as
                                      nb = fromIntegral $ length bs
                                      n  = na + nb
                                  in (na/n) * dataEntropy as + (nb/n) * dataEntropy bs
          dataEntropy = entropy . map fst

-- Builds question masks and data splits along the tree, maps them
-- with the passed function, then returns an accumulation
-- mapPaths :: QTree -> EncData -> (EncData -> [Int] -> a) -> [(a, Path)]

-- filter a data set along a path through a QTree
-- treeFilter :: QTree -> Path -> Data -> Data


--
-- Test data

tree1 :: BT.BTree Char
tree1 = fmap (head . Set.toList) $ read "((  (e (u (o (i a))))) (h ((n (l (x r))) ((y (s (g d))) (((t k) (w c)) ((p (b (q j))) (f (m (z v)))))))))"

bsenc :: Map.Map Char Path
bsenc = BT.bsEncode tree1

splitData :: Double -> [a] -> ([a], [a])
splitData percentage dat = splitAt (ceiling $ (fromIntegral n)*percentage) dat
  where n = length dat

-- Get the development/validation data sets        
(dev, cross) = splitData 0.8 train
fourgram :: [d] -> [(d, d, d, d)]
fourgram os = zip4 os (tail os) (tail $ tail os) (tail $ tail $ tail os)

-- Split the data up into fourgrams
dev4g, cross4g :: EncData
Just dev4g = bitPredicts bsenc $ fourgram dev
Just cross4g = bitPredicts bsenc $ fourgram cross
