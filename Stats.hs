module Stats (Freq(totalCount), freqFrom, freqOf, countOf, log2, entropy) where

import Foreign.C.Types
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe (fromMaybe)


-- The frequency data type encapsulates sample counts and frequencies
data Freq a = Freq { totalCount :: Integer, counts :: M.Map a Integer }

-- For any given container which can be folded over, produce a sample
-- distribution of the objects it contains
freqFrom :: (F.Foldable t, Ord a) => t a -> Freq a
freqFrom cont = Freq n $ F.foldr (M.alter (Just . maybe 1 (+1))) M.empty cont
    where n = F.foldr (\_ -> (+1)) 0 cont

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
