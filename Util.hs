module Util where

import DTree ( DTree(..) )

dTreeJSON :: Show a => DTree s a -> (s -> String) -> String
dTreeJSON (Leaf x) _ = "var dtree = " ++ show x
dTreeJSON tr showq = "var dtree = " ++ printDTree' tr
    where
      printDTree' (Leaf _) = "1"
      printDTree' (Branch q (Leaf _) (Leaf _)) = 
          "{name: '" ++ showq q ++ "'}"
      printDTree' (Branch q l (Leaf _)) = 
          "{name: '" ++ showq q ++ "', left: " ++ printDTree' l ++ "}"
      printDTree' (Branch q (Leaf _) r) = 
          "{name: '" ++ showq  q ++ "', right: " ++ printDTree' r ++ "}"
      printDTree' (Branch q l r) = 
          "{name: '" ++ showq q ++ "', left: " ++ printDTree' l ++ 
                         ", right: " ++ printDTree' r ++ "}"

showBSQ :: (Int, Int) -> String
showBSQ (i, j) = show (3-i) ++ ":" ++ show j
