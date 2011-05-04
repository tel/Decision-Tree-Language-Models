{-# LANGUAGE ForeignFunctionInterface #-}

module Text where

import Foreign.C.Types

import Data.List
import Data.Maybe (fromMaybe, maybe)
import qualified Data.Map as Map
import Data.Foldable (foldMap, Foldable)
import Data.Monoid (Sum (..), getSum)
import System.IO.Unsafe

data Frequency a = Freq {fromFreq :: (Map.Map a Double)}
type Bigram = (Char, Char)

foreign import ccall unsafe "math.h log2"
  c_log2 :: CDouble -> CDouble
             
log2 :: Double -> Double
log2 x = realToFrac (c_log2 (realToFrac x))

entropy :: Ord a => [a] -> Double
entropy = entropyf . obj_freq

entropyf :: Frequency a -> Double
entropyf freq = getSum $ foldMap fn (fromFreq freq)
  where fn p = Sum $ -p * log2 p

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

freqOf :: Ord o => o -> Frequency o -> Double
freqOf o (Freq f) = fromMaybe 0 (Map.lookup o f)