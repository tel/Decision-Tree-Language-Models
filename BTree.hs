module BTree (
  BTree(Split, Leaf), 
  Motion(TLeft, TRight), 
  Path(Path, unPath), 
  emptyPath, bsEncode) 
       where

import Data.Foldable
import Data.Functor
import Data.Monoid
import qualified Data.Map as Map

import Motion

data BTree a = Split (BTree a) (BTree a)
             | Leaf a
               deriving Eq
                        
instance Foldable BTree where                        
  foldMap f (Leaf x) = f x
  foldMap f (Split y z) = foldMap f y `mappend` foldMap f z
  
instance Functor BTree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Split l r ) = Split (fmap f l) (fmap f r)
  
bsEncode :: Ord a => BTree a -> Map.Map a Path
bsEncode tree = Map.fromList $ zeroPad $ encodings tree emptyPath
  where encodings :: BTree a -> Path -> [(a, Path)]
        encodings (Leaf f) bs = [(f, bs)]
        encodings (Split l r) (Path bs) = 
          encodings l (Path $ TLeft:bs) ++ encodings r (Path $ TRight:bs)
        zeroPad :: [(a, Path)] -> [(a, Path)]
        zeroPad enc = let l = Prelude.maximum $ map (length . unPath . snd) enc
                      in map (\(a, Path bs) -> 
                               (a, Path $ reverse $ replicate (l - length bs) TLeft ++ bs)) 
                         enc
 