module Main where

import Data.List (maximumBy, partition)
import Data.Ord (comparing)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

import Stats

alphabet = "abcdefghijklmnopqrstuvwxyz "
types = [0..2^(27)]

splitByType ty = (a, b)
    where
      bytes = showIntAtBase 2 intToDigit ty ""
      (a, b) = (map fst at, map fst bt)
      (at, bt) = partition ((=='1') . snd) (zip alphabet (bytes ++ repeat '0'))

main = do tr <- readFile "textA.txt" 
          let fr = freqFrom tr
              frq x = freqOf x fr
              splitEntropy (a, b) = (na/n) * ha + (nb/n) * hb
                  where na = fromIntegral $ length a
                        nb = fromIntegral $ length b
                        n = na + nb
                        ha = sum $ map surprise a
                        hb = sum $ map surprise b
                        surprise x = let p = frq x in p * log2 p

          print $ maximumBy (comparing $ splitEntropy) (map splitByType types)