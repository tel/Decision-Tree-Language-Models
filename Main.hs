{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as F
import Data.List
import Text.ParserCombinators.Parsec
import Data.Monoid

import qualified ClusterTree as C
import BTree
import Text
import Agglomeration

-- main = print $ agglom uf bf C.singletons

tree1 :: BTree Char
tree1 = fmap (head . Set.toList) $ read "((  (e (u (o (i a))))) (h ((n (l (x r))) ((y (s (g d))) (((t k) (w c)) ((p (b (q j))) (f (m (z v)))))))))"

bsenc = bsEncode tree1

splitData :: Float -> [a] -> ([a], [a])
splitData percentage dat = splitAt (ceiling $ (fromIntegral n)*percentage) dat
  where n = length dat
        
-- Get the development/validation data sets        
(dev, cross) = splitData 0.8 train
fourgram :: [d] -> [(d, d, d, d)]
fourgram os = zip4 os (tail os) (tail $ tail os) (tail $ tail $ tail os)
bitPredicts
  :: Ord k => Map.Map k Path -> [(k, k, k, t)] -> Maybe [(t, Path)]
bitPredicts bs = mapM fn
  where fn (a, b, c, d) = do Path aenc <- Map.lookup a bs
                             Path benc <- Map.lookup b bs
                             Path cenc <- Map.lookup c bs
                             return $ (d, Path $ cenc ++ benc ++ aenc)

Just dev4g = bitPredicts bsenc $ fourgram dev
Just cross4g = bitPredicts bsenc $ fourgram cross