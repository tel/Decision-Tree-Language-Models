{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import  Data.Foldable (foldMap)
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
newtype Bitstream = Bitstream { bitstreamArray :: (Array EncIx Motion) }

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
data QTree = Split EncIx QTree QTree | Leaf deriving (Show, Eq)

qAbove :: Int -> QTree -> [[EncIx]]
qAbove n tree = reverse $ rep n [tree] []
    where rep 0 _ lst = lst
          rep n trees lst = let (lst', trees') = ((: lst) . concat *** (++ trees) . concat) $ 
                                                 unzip (map trim trees)
                            in rep (n-1) trees' lst'
          trim Leaf = ([], [])
          trim (Split ix l r) = ([ix], [l, r])

-- Splits the data into two sets based on whether each item's
-- predictor at EncIx is TLeft (fst) or TRight (snd)
splitDataWith :: EncData -> EncIx -> (EncData, EncData)
splitDataWith dat ix = partition fn dat
    where fn (_, Bitstream arr) = arr ! ix == TLeft


-- unfoldQTree
-- Build a QTree recursively using the maybeSplit function.
-- 
unfoldQTree :: (EncData, EncData)   -- Development and Held Out data
            -> ((EncData, EncData)  -- Locally available dev and ho data
                    -> [Int]        -- Admissible question key
                    -> Maybe EncIx) -- If we'll divide, this is the
                                    -- split to do it by
            -> QTree
unfoldQTree (dev, ho) maybeSplit = rep (dev, ho) (replicate n 0)
    where ((_, _), (n, _)) = bounds $ bitstreamArray $ snd $ head dev
          rep (dev, ho) curkey = 
              case maybeSplit (dev, ho) curkey of
                Nothing -> Leaf
                Just ix@(i, j) -> Split ix lsplit rsplit
                    where lsplit = rep (ldev, lho) newkey
                          rsplit = rep (rdev, rho) newkey
                          (ldev, rdev) = splitDataWith dev ix
                          (lho, rho) = splitDataWith ho ix
                          newkey = take i curkey ++ [(curkey !! i) + 1] ++ drop (i+1) curkey

-- Creates a tree from a given data set
devPercent = 0.8 :: Float
crossEntropyCutoff = 0:: Double
buildTree :: EncData -> QTree
buildTree dat = unfoldQTree (splitData devPercent dat) evaluator
    where exampleArr = bitstreamArray $ snd (head dat)
          realQp = inRange (bounds exampleArr)
          splitData :: Float -> [a] -> ([a], [a])
          splitData percentage dat = 
              splitAt (ceiling $ (fromIntegral $ length dat)*percentage) dat
          evaluator :: (EncData, EncData) -> [Int] -> Maybe EncIx
          evaluator (dev0, ho0) key = case permisQ of
                                        [] -> Nothing
                                        _  -> case cE0 - cE1 > crossEntropyCutoff of
                                                -- This scaling might
                                                -- not be correct. We
                                                -- might need to
                                                -- propagate the
                                                -- current frequency
                                                -- of this equivalence
                                                -- class and
                                                -- divide. Also cE1
                                                -- seems to be zero'd.
                                                False -> Nothing
                                                True -> Just bestQ
              where permisQ = filter realQp $ zip [0..] key
                    bestQ = fst $ 
                            minimumBy (comparing snd) $ 
                            map (id &&& splitEntropy . splitDataWith dev0) permisQ
                    splitEntropy (dev1, dev2) = (n1/n) * entropy (map fst dev1) + 
                                                (n2/n) * entropy (map fst dev2)
                        where n1 = fromIntegral $ length dev1
                              n2 = fromIntegral $ length dev2
                              n  = n1 + n2
                    crossEntropy ho dev0 = let g = obj_freq (map fst ho0)
                                               f = obj_freq (map fst dev0)
                                               fn w = Sum $ -freqOf w g * (log $ freqOf w f)
                                           in getSum $ foldMap fn alphabet
                    cE0 = crossEntropy ho0 dev0
                    (bdev1, bdev2) = splitDataWith dev0 bestQ
                    (bho1, bho2) = splitDataWith ho0 bestQ
                    n1 = fromIntegral $ length bho1
                    n2 = fromIntegral $ length bho2
                    n  = n1 + n2
                    cE1 = (n1/n) * (crossEntropy bho1 bdev1) +
                          (n2/n) * (crossEntropy bho2 bdev2)

--
-- Test data

tree1 :: BT.BTree Char
tree1 = fmap (head . Set.toList) $ read "((  (e (u (o (i a))))) (h ((n (l (x r))) ((y (s (g d))) (((t k) (w c)) ((p (b (q j))) (f (m (z v)))))))))"

bsenc :: Map.Map Char Path
bsenc = BT.bsEncode tree1

-- Get the development/validation data sets        
fourgram :: [d] -> [(d, d, d, d)]
fourgram os = zip4 os (tail os) (tail $ tail os) (tail $ tail $ tail os)

-- Split the data up into fourgrams
Just train4g = bitPredicts bsenc $ fourgram train

t = buildTree train4g