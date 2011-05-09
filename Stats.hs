module Stats 
    ( Freq(Freq, totalCount, counts), 
      freqFrom, marginalize, imap,
      freqOf, countOf, 
      log2, entropy,
      charFreqEntropy, freqFromC ) where

import Foreign.C.Types
import Control.Monad (liftM)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Char (ord)

-- The frequency data type encapsulates sample counts and frequencies
data Freq a = Freq { totalCount :: Integer, counts :: M.Map a Integer }

-- For any given container which can be folded over, produce a sample
-- distribution of the objects it contains
-- freqFrom :: (F.Foldable t, Ord a) => t a -> Freq a
-- freqFrom cont = Freq n $ F.foldl' (flip $ M.alter (Just . maybe 1 (+1))) M.empty cont
--     where n = F.foldl' (\a _ -> (a+1)) 0 cont
freqFrom :: (F.Foldable t, Ord a) => t a -> Freq a
freqFrom cont = Freq n $ F.foldl' (\map key -> M.insertWith (+) key 1 map) M.empty cont
    where n = F.foldl' (\a _ -> (a+1)) 0 cont

-- Given a Freq, we should be able to poll it for frequencies and
-- counts. These are supposed to mimic the interface of M.lookup,
-- although for unknown keys they default to 0. Sensible!
countOf :: Ord a => a -> Freq a -> Integer
freqOf :: Ord a => a -> Freq a -> Double

countOf o fr = fromMaybe 0 (M.lookup o (counts fr))
freqOf  o fr = (fromIntegral $ countOf o fr) / (fromIntegral $ totalCount fr)


-- A real log2 implementation so that we can deal in bits
foreign import ccall unsafe "math.h log2"
  c_log2 :: CDouble -> CDouble
log2 :: Double -> Double
log2 x = realToFrac (c_log2 (realToFrac x))

-- and an entropy defined over `Freq`s
entropy :: Freq o -> Double
entropy fr = log2 n - 1/n * 1/(F.sum $ fmap fn (counts fr))
  where n = fromIntegral $ totalCount fr
        fn :: Integer -> Double
        fn = ((\c -> c * (log2 c)) . fromIntegral)


-- since computing the entropy of character distributions is common,
-- here's an implementation which abuses ordinal mapping to speed up
-- the computation (a lot!)
charFreqEntropy :: [Char] -> Double
charFreqEntropy chars = intMapEntropy (freqFromC ords)
   where ords = map ord chars

intMapEntropy :: (Integer, IM.IntMap Integer) -> Double
intMapEntropy (n, fr) = log2 numn - 1/numn * 1/(F.sum $ fmap fn fr)
  where numn = fromIntegral n
        fn :: Integer -> Double
        fn = ((\c -> c * (log2 c)) . fromIntegral)

freqFromC :: [Int] -> (Integer, IM.IntMap Integer)
freqFromC cont = (n, foldl' fn IM.empty cont)
    where n = fromIntegral $ length cont
          fn :: IM.IntMap Integer -> Int -> IM.IntMap Integer
          fn map char = IM.insertWith (+) char 1 map
                            
-- (flip $ M.alter (Just . maybe 1 (+1)))

-- compute a marginal frequency from a given frequency such that
-- p2(b) = \sum_{a : a -> b} p1(a)
-- use with (fst) and (snd) to get the usual marginals
--
-- Technically note that this definition is sufficient to enter Freq
-- into the Functor typeclass so long as the underlying type a is
-- Ord. Since Haskell doesn't allow that directly we'll just also
-- define item map, imap
marginalize :: Ord b => (a -> b) -> Freq a -> Freq b
marginalize group (Freq n cts) = Freq n $ F.foldl' fn M.empty (M.toList cts)
        where fn map (k, v) = M.alter (Just . fromMaybe v . liftM (+v)) (group k) map

imap :: Ord b => (a -> b) -> Freq a -> Freq b
imap = marginalize
