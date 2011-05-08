module Main where

import System.IO.Unsafe (unsafePerformIO)

import Control.Arrow
import Data.Array (Array(..), bounds, (!), listArray)
import Data.Ix
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as M
import Data.List (minimumBy, maximumBy, zip4)

import Agglomeration
import Text
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
    then Branch bestQ (splitNode (devl, hol) (hdev', hho') qlst)
                      (splitNode (devr, hor) (hdev', hho') qlst)
    else Leaf ()
    where
      hho'  = splitEntropy ho bestQ
      hdev' = splitEntropy dev bestQ
      (devl, devr) = splitByQ bestQ dev
      (hol, hor) = splitByQ bestQ ho
      (bestQ, qlst) = minimumBy (comparing $ splitEntropy dev . fst) qs
      qs = [((head qs1), ((tail qs1), qs2, qs3)),
            ((head qs2), (qs1, (tail qs2), qs3)),
            ((head qs3), (qs1, qs2, (tail qs3)))]

splitList :: Double -> [a] -> ([a], [a])
splitList percent lst = splitAt n lst
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

makeObs :: DTree s Char -> String -> Maybe [Obs]
makeObs tree str = mapM fn fg
    where bs = pathMap tree
          fg = zip4 str (tail str) (drop 2 str) (drop 3 str)
          fn (a, b, c, d) = do aenc <- M.lookup a bs
                               benc <- M.lookup b bs
                               cenc <- M.lookup c bs
                               let n = length aenc
                                   full = listArray ((0, 0), (2, (n-1))) 
                                          (cenc ++ benc ++ aenc)
                               return $ (d, full)

train = unsafePerformIO (readFile "textA.txt")
train2g = bigrams train
obs = fromJust $ makeObs tree1 train
tr = buildTree 0.8 obs

printDTree :: Show a => DTree (Int, Int) a -> String
printDTree (Branch s l r) = "'" ++ show s ++ "': {" ++ printDTree l ++ ", " ++ printDTree r ++ "}"
printDTree (Leaf a) = "leaf: " ++ show a

mapQlists :: DTree (Int, Int) a -> [[(Int, Int)]]
mapQlists tr = fn tr []
    where fn (Leaf _) qlst = [reverse qlst]
          fn (Branch s l r) qlist = let qlist1 = s:qlist in fn l qlist1 ++ fn r qlist1

qlists = zipWith3 (\a b c -> show a ++ ", " ++ show b ++ ", " ++ show c) (concat qs) ids ts
    where qs = map (map (\(a,b) -> 9*a + b)) $ mapQlists tr
          ids = concat . zipWith ($) (map (replicate . length) qs) $ [1..270]
          ts = concat $ map (\a -> [1..(length a)]) qs

qlists2 = map (\idx -> fromMaybe 0 $ M.lookup idx jumpC) (range ((0, 0), (26, 26)))
    where qs = map (map (\(a,b) -> 9*a + b)) $ mapQlists tr
          jumpC = counts $ freqFrom $ concat $ map (\lst -> zip lst (tail lst)) qs
main = do train <- readFile "textA.txt"
          let train2g   = bigrams train
              bg        = freqFrom train2g
              alpha     = "abcdefghijklmnopqrstuvwxyz "
              obs       = fromJust $ makeObs tree1 train
          writeFile "jump_counts.csv" (unlines $ map show qlists2)