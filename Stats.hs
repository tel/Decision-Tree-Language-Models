{-# Language
  MultiParamTypeClasses,
  FunctionalDependencies,
  FlexibleInstances,
  OverlappingInstances
  #-}

module Stats 
    ( Freq, 
      freqFrom, domain, freqOf, entropy, gini,
      log2 ) where

import Foreign.C.Types
import Control.Monad (liftM)
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Char (ord, chr)


-- Firstly, a real log2 implementation so that we can deal in bits
foreign import ccall unsafe "math.h log2"
  c_log2 :: CDouble -> CDouble
log2 :: Double -> Double
log2 x = realToFrac (c_log2 (realToFrac x))

-- The frequency class encapsulates types that map over data spaces
-- and return measure 1 reals.
class Freq f a | a -> f where
    freqFrom :: [a] -> f a
    domain   :: f a -> [a]
    freqOf   :: a -> f a -> Double
    entropy  :: f a -> Double
    gini     :: f a -> Double

    -- Default entropy implementation
    entropy fr = -sum (map surprise (domain fr))
        where surprise x = let c = freqOf x fr
                           in c * (log2 c)

    -- Default gini index
    gini fr = 1 - sum (map sqprob (domain fr))
        where sqprob x = let c = freqOf x fr
                         in c**2

-- A general Data.Map instance of Freq. If we don't override this on a
-- particular type, it'll act as a fall-back. Unfortunately, general
-- map lookups and alters are SLOW.
--
-- In order to avoid overlapping instances, there's a toolkit for
-- building new instances generically.
data FreqMap a = FreqMap (M.Map a Double)
genericFreqFrom os = 
    FreqMap (foldl' (\map key -> M.insertWith (+) key quanta map) M.empty os)
        where quanta = (1 :: Double)/(fromIntegral $ length os)
genericDomain (FreqMap map) = M.keys map
genericFreqOf o (FreqMap map) = fromMaybe 0 $ M.lookup o map

-- For instance, in order to chart sets of objects
instance Ord a => Freq FreqMap (S.Set a) where
    freqFrom = genericFreqFrom
    domain = genericDomain
    freqOf = genericFreqOf

instance Ord a => Freq FreqMap (a, a) where
    freqFrom = genericFreqFrom
    domain = genericDomain
    freqOf = genericFreqOf

-- A more specific mapping so long as we're looking for entropies over
-- characters. The a is a phantom parameter.
--
data CountMap a = CountMap {toIntFn :: a -> IM.Key, 
                            fromKeyFn :: IM.Key -> a,
                            totalCount :: Int, 
                            counts :: IM.IntMap Integer}

instance Freq CountMap Char where
    freqFrom cs = CountMap ord chr n $ foldl' fn IM.empty (map ord cs)
        where n = fromIntegral $ length cs
              fn map i = IM.insertWith (+) i 1 map
    domain cm = map (fromKeyFn cm) $ IM.keys (counts cm)
    freqOf o cm = numer/denom
        where numer = fromIntegral $ fromMaybe 0 (IM.lookup (ord o) (counts cm))
              denom = fromIntegral $ totalCount cm