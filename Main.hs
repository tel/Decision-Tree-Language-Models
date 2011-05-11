module Main where

import System.IO.Unsafe (unsafePerformIO)

import Control.Arrow
import Data.Array (Array(..), bounds, (!), listArray)
import Data.Ix
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as M
import Data.List (minimumBy, maximumBy, zip4)
import Data.Char (ord)
import qualified Data.IntMap as IM

import Agglomeration
import Stats
import DTree
-- Alright.                      

-- So lets start the DTree
type Predictors = Array (Int, Int) Dir

type Obs = (Char, Predictors)
type Q = (Int, Int)
type QList = ([Q], [Q], [Q])

buildTree perc obs = splitNode (dev, ho) (hdev, hho) defaultQList
    where (dev, ho) = splitList perc obs 
          hdev = charFreqEntropy $ map fst dev
          hho = charFreqEntropy $ map fst ho

reductionThreshold = 0.005
-- splitNode
--   :: ([Obs], [Obs])
--      -> (Double, Double)
--      -> ([(Int, Int)], [(Int, Int)], [(Int, Int)])
--      -> DTree (Int, Int) Double
splitNode (dev, ho) (hdev, hho) (qs1, qs2, qs3) = 
    if hho - hho' > reductionThreshold
    then Branch (bestQ, freq) (splitNode (devl, hol) (hdev', hho') qlst)
                              (splitNode (devr, hor) (hdev', hho') qlst)
    else Leaf freq
    where
      freq  = freqFromC $ map (ord . fst) all
      all   = ho ++ dev
      hho'  = splitEntropy ho bestQ
      hdev' = splitEntropy dev bestQ
      (devl, devr) = splitByQ bestQ dev
      (hol, hor) = splitByQ bestQ ho
      (bestQ, qlst) = minimumBy (comparing $ splitEntropy dev . fst) qs
      qs = [((head qs1), ((tail qs1), qs2, qs3)),
            ((head qs2), (qs1, (tail qs2), qs3)),
            ((head qs3), (qs1, qs2, (tail qs3)))]

splitList :: Double -> [a] -> ([a], [a])
splitList percent lst = 
    where n = ceiling $ (fromIntegral $ length lst) * percent

oneSide :: [Obs] -> Double -> (Int, Int) -> Double
oneSide obs h q = h - h'
    where h' = dualEntropy (splitByQ q obs)

defaultQList :: QList
defaultQList = (ls 0, ls 1, ls 2)
    where ls a = zip (repeat a) [0..]

splitByQ :: (Int, Int) -> [Obs] -> ([Obs], [Obs])
splitByQ idx ngrams = (a, b)
    where a = filter (\(_, enc) -> look enc == L) ngrams
          b = filter (\(_, enc) -> look enc == R) ngrams
          look enc = if inRange (bounds enc) idx then enc ! idx else L

dualEntropy :: ([Obs], [Obs]) -> Double
dualEntropy (left, right) = (nl/n)*hleft + (nr/n)*hright
    where hleft  = charFreqEntropy $ map fst left
          nl     = fromIntegral $ length left
          hright = charFreqEntropy $ map fst right
          nr     = fromIntegral $ length right
          n      = nl + nr

splitEntropy :: [Obs] -> (Int, Int) -> Double
splitEntropy obs idx = dualEntropy (splitByQ idx obs)

--
-- Main things
--
tree1 :: BTree Char
tree1 = read "(((((a o) (i u)) e)  ) ((((((((v z) k) m) f) (((j q) b) p)) ((c w) t)) h) ((((r x) l) n) ((s y) (d g)))))"

train = unsafePerformIO (readFile "textA.txt")
train2g = bigrams train
obs = fromJust $ makeObs tree1 train
tr = buildTree 0.8 obs

test = unsafePerformIO (readFile "textB.txt")
test2g = bigrams test
obs_test = fromJust $ makeObs tree1 test

main = do train <- readFile "textA.txt"
          let train2g   = bigrams train
              bg        = freqFrom train2g
              alpha     = "abcdefghijklmnopqrstuvwxyz "
              obs       = fromJust $ makeObs tree1 train
              ntest     = fromIntegral $ length test
          print $ 2**(-(sum $ map (lik tr) obs_test)/ntest)