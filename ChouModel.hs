{-# LANGUAGE
  TypeSynonymInstances,
  MultiParamTypeClasses,
  FlexibleContexts
  #-}

-- Chou's Method decision tree language model
module Main where

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.IntMap as IM
import Data.List (partition, foldl', nub, sortBy)
import Data.Ord (comparing)
import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)
import Data.Array (Array, accumArray, (!))

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
-- defaultSplits = ("acegikmoqsuwy ", "bdfhjlnprtvxz")


-- Then, we'll perform an update for each index
proposeSplits seed = map (chouImprove defaultSplits seed) [0..(n-1)]
    where n = length $ snd $ head seed

-- We'll introduce the domain of letters...
alphabet = "abcdefghijklmnopqrstuvwxyz "

-- And a strictly ordered mapping of this domain
myOrd :: Char -> Int
myOrd ' ' = ord 'z' + 1
myOrd c = ord c

myOrdRange = ((myOrd 'a', myOrd 'a'), (myOrd ' ', myOrd ' '))

chouImprove :: (String, String) -> Seed -> Int-> Q
chouImprove (a0, nota0) obs idx = chouImprove' (a0, nota0)
    where 
      -- Find the local domain
      alphabet = sortBy (comparing myOrd) $ nub $ map fst obs
      -- Define some distributions
      n  = fromIntegral $ length obs
      fwb :: Array (Int, Int) Double -- f(w, beta)
      fwb = accumArray (\count item -> count + (1/n)) 
            0 myOrdRange $ 
            map (\(w, pred) -> 
                     ((myOrd w, myOrd (pred !! idx)), w)) 
            obs
      fb :: Array Int Double         -- f(beta)
      fb = accumArray (\count item -> count + (1/n)) 
           0 (myOrd 'a', myOrd ' ') $
           map (\(w, pred) -> 
                     (myOrd (pred !! idx), w))
           obs
      -- Wrap the accessor so we don't have to manage the ordinal mapping
      getfwb :: Char -> Char -> Double
      getfwb w beta = fwb ! (myOrd w, myOrd beta)
      getfb :: Char -> Double
      getfb beta = fb ! (myOrd beta)
      -- This also gets us the conditional, w given b
      getfwgb :: Char -> Char -> Double
      getfwgb w beta = if getfb beta /= 0 then (getfwb w beta)/(getfb beta) 
                       else 0
      -- Define the set mapping f(w | set)
      getfset :: Char -> String -> Double
      getfset w set = if (sum $ map getfb set) /= 0 
                      then (sum $ map (getfwb w) set)/(sum $ map getfb set)
                      else 0
      -- And now we can define the Gini index derived score we're trying to minimize
      score :: String -> Char -> Double
      score set beta = sum $ map (\w -> (getfset w set - getfwgb w beta)**2) alphabet
      -- And that gives us the Chou improvement algorithm
      chouImprove' (a, nota) =
          let (a', nota') = 
                  partition (\x -> score a x <= score nota x) alphabet
          in if a == a' then
                 Q idx a' nota'
             else
                 chouImprove' (seq (unsafePerformIO $ print (idx, a', nota')) a', nota')
             

          
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


main = do tr <- readFile "textA.txt"
          sequence $ map print $ proposeSplits (makeObs tr)