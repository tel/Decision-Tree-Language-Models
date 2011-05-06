module Main where

import Agglomeration
import Text
import Stats
import DTree
-- Alright.                      

-- So lets start the DTree




--
-- Main things
--
tree1 :: BTree Char
tree1 = read "(((((a o) (i u)) e)  ) ((((((((v z) k) m) f) (((j q) b) p)) ((c w) t)) h) ((((r x) l) n) ((s y) (d g)))))"

main = do train <- readFile "textA.txt"
          let train2g = bigrams train
              bg      = freqFrom train2g
              alpha   = "abcdefghijklmnopqrstuvwxyz "
          print $ tree1