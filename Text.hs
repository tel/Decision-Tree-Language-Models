module Text where

import Data.List
import Data.Maybe (fromMaybe, maybe)
import qualified Data.Map as Map
import System.IO.Unsafe

data Frequency a = Freq {fromFreq :: (Map.Map a Float)}
type Bigram = (Char, Char)

-- Load the actual corpuses. Totally unsafe but a good start.
train = unsafePerformIO $ readFile "textA.txt"
test  = unsafePerformIO $ readFile "textB.txt"

-- Get the sliding window bigrams of a bytestring
bigrams :: String -> [Bigram]
bigrams str = zip str (tail str)

obj_counts :: (Num a, Ord o) => [o] -> Map.Map o a
obj_counts = foldl' mapAcc Map.empty
  where mapAcc map obj = Map.alter (Just . maybe 1 (+ 1)) obj map
        
obj_freq :: Ord o => [o] -> Frequency o
obj_freq os = Freq $ Map.map (/ (fromIntegral n)) (obj_counts os)
  where counts = obj_counts os
        n      = length os
        
-- Base language
alphabet = "abcdefghijklmnopqrstuvwxyz "

--
-- Some statistics
unigram_f :: String -> Frequency Char        
unigram_f = obj_freq        

bigram_f :: String -> Frequency Bigram
bigram_f = obj_freq . bigrams

freqOf :: Ord o => o -> Frequency o -> Float
freqOf o (Freq f) = fromMaybe 0 (Map.lookup o f)