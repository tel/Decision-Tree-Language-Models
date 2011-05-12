{-# LANGUAGE
  TypeSynonymInstances,
  MultiParamTypeClasses,
  FlexibleContexts
  #-}

-- Chou's Method decision tree language model
module Main where

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.IntMap as IM
import Data.List (partition, foldl')
import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)

import Stats
import DTree
import Util

-- Here we store 4-grams unchanged. The predictor is just a list of
-- the preceeding letters
type Pred = String

-- Chou-style questions involve both an atomic breakdown, here stored
-- as the character index to separate on, and divisions of those atoms
-- into left and right selections.
data Q = Q { chouIndex :: Int,
             leftSet   :: String,
             rightSet  :: String }
         deriving (Eq, Show)

-- instance Show Q where
--     show (Q idx _ _) = show idx

-- Now we can set up the splitter instance
instance Splitter Pred Q where
    sfilter q pred = let l = pred !! (chouIndex q) in elem l (leftSet q)

-- The seed is far more complex though. We'll actually pass along a
-- development set of observations which get trimmed down and used to
-- do the Chou-style A/not-A improvements
type Seed = [(Char, String)]

-- Updating the seed just involves filtering the seed by whatever
-- question was chosen
updateSeed :: Seed -> Q -> Dir -> Seed
updateSeed seed q dir = let (left, right) = partition (sfilter q . snd) seed
                        in case dir of 
                             L -> left
                             R -> right

-- The real magic is all in the proposal function. Here we intend to
-- return three choices of Q, one for each letter in the history,
-- after performing a Chou update for each one.

proposeSplits :: Seed -> [Q]

-- Introducing this function requires answering questions about how to
-- choose the initial A/not-A division. For the moment, we'll take a
-- cue from the agglomerative model and split it by vowel/consonant to
-- start.

defaultSplits = ("aeiou ", "bcdfghjklmnpqrstvwxyz")

-- Then, we'll perform an update for each index
-- proposeSplits seed = map (chouImprove defaultSplits seed) [0..(n-1)]
--     where n = length $ snd $ head seed
proposeSplits = undefined

alphabet = "abcdefghijklmnopqrstuvwxyz "

newtype M = M Int
instance Show M where
    show (M 0) = ""
    show (M i) = "I" ++ show (M (i-1))

chouImprove :: (String, String) -> Seed -> Int-> Q
chouImprove (a0, nota0) obs idx = chouImprove' (a0, nota0)
    where chouImprove' (a, nota) =
              let (a', nota') = 
                      {-# SCC "part" #-} partition (\x -> score a x <= score nota x) alphabet
              in if a == a' then
                     Q idx a' nota'
                 else
                     chouImprove' ((seq (unsafePerformIO $ print (a', nota')) a'), nota')
          n  = fromIntegral $ length obs
          betasets = foldl' (\map (l, pred) -> 
                                 IM.insertWith (++)
                                       (ord (pred !! idx)) [l] map) 
                     IM.empty obs
          set beta = fromMaybe [] $ IM.lookup (ord beta) betasets
          condfreq beta = freqFrom $ set beta
          condfr beta x = freqOf x (condfreq beta)
          ct beta = length $ set beta
          frb beta = (fromIntegral $ ct beta)/n
          fra set x = (sum $ map (\l -> condfr l x * frb l) set)/(sum $ map frb set)
          score a x = sum $ map (\beta -> (condfr beta x - fra a x)**2) alphabet
                    

          
growChouModel :: [(Char, Pred)] -> DTree Q Double
growChouModel obs = growTree
                    obs 0.8
                    proposeSplits
                    updateSeed
                    obs
                    (gini . freqFrom)
                    (gini . freqFrom)
                    (\old new -> old - new > 0.005)

makeObs :: String -> [(Char, Pred)]
makeObs str = zip str (zipWith3 (\a b c -> [a, b, c]) 
                                    (tail str)
                                    (tail $ tail str)
                                    (tail $ tail $ tail str))


tr = unsafePerformIO $ readFile "textA.txt"

main = print $ chouImprove defaultSplits (makeObs tr) 2