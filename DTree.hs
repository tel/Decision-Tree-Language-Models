{-# LANGUAGE 
  FlexibleInstances, 
  OverlappingInstances, 
  TypeSynonymInstances,
  ExistentialQuantification,
  MultiParamTypeClasses,
  FunctionalDependencies #-}
module DTree 
    ( DTree(..), BTree, bBranch,
      Dir(..),
      flatten, pathMap ) where

-- Alright.
import qualified Data.Foldable as F (Foldable, foldr, foldMap, maximum)
import qualified Data.Map as M
import Data.List (partition)
import Data.Traversable (Traversable, traverse)
import Data.Monoid (Monoid)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Text.Read (lift, readPrec)
import Text.ParserCombinators.ReadP

-- The general decision tree type. It's particularly important to note
-- that types can be stored at both branch and leaf nodes (important
-- for storing the questions) although a simpler interface exists as
-- BTree which defaults () to the branch type. This branch and leaf
-- typing allows for trees to store meaningful and different data at
-- each position, which is important in sharing the structure of tree
-- for both filtration and likelihood estimation at the conditionals.
data DTree s a = Branch s (DTree s a) (DTree s a)
               | Leaf a
                 deriving (Eq, Ord)
type BTree a = DTree () a

-- A branch overloader for BTrees
bBranch :: BTree a -> BTree a -> BTree a
bBranch = Branch ()

--
-- A few kinds of overloaded shows. Not all will be `Read`able, but this
-- will help for debugging.
instance (Show a, Show s) => Show (DTree s a) where
    show (Leaf a) = show a
    show (Branch s l r) = "(" ++ show s ++ "| " ++ show l ++ " " ++ show r ++ ")"

instance (Show a) => Show (DTree () a) where
    show (Leaf a) = show a
    show (Branch _ l r) = "(" ++ show l ++ " " ++ show r ++ ")"

instance Show (DTree () Char) where
    show (Leaf c) = [c]
    show (Branch _ l r) = "(" ++ show l ++ " " ++ show r ++ ")"

--
-- The parser for BTree Char
parseBTC :: ReadP (BTree Char)
parseBTC = parseLeaf +++ parseBranch
    where parseLeaf = (liftM Leaf) $ satisfy (`elem` "abcdefghijklmnopqrstuvwxyz ")
          parseBranch = do char '('
                           l <- parseBTC
                           char ' '
                           r <- parseBTC
                           char ')'
                           return $ Branch () l r

instance Read (BTree Char) where
    readPrec = lift parseBTC

-- 
-- Define some useful control instances
instance Functor (DTree s) where
    f `fmap` (Leaf x) = Leaf (f x)
    f `fmap` (Branch s l r) = Branch s (f `fmap` l) (f `fmap` r)

flatten :: Monoid a => DTree s a -> a
flatten = F.foldMap id

instance F.Foldable (DTree s) where
    foldr fn a0 (Leaf x) = fn x a0
    foldr fn a0 (Branch _ l r) = F.foldr fn (F.foldr fn a0 r) l

instance Traversable (DTree s) where
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Branch s l r) = Branch s <$> traverse f l <*> traverse f r

--
-- Path functions

data Dir = L | R deriving (Eq, Show, Ord)
type Path = [Dir]

pathMap :: Ord a => DTree s a -> M.Map a Path
pathMap tree = normalize (pathMap' tree [])
    where pathMap' (Leaf x) path = M.singleton x (reverse path)
          pathMap' (Branch _ l r) path = (pathMap' l (L:path)) `M.union` (pathMap' r (R:path))
          normalize m = let n = F.maximum $ M.map length m
                        in M.map (\a -> a ++ replicate (n - length a) L) m

--
-- Data-driven growth functionality
--
-- Here decision tree growth is generalized such that each node
-- splitting decision involves a split proposal step (based optionally
-- on a spliter seed state which updates along paths in the growing
-- tree), a split selection step (which is fully data driven and
-- ignorant of the actually split condition), and then a split
-- evaluation step (which is fully data driven off of held-out data)
-- which returns a "goodness" score of each split of the data set. If
-- the count-weighted average of these "goodnesses" doesn't beat the
-- "goodness" of the node pre-split, it is considered terminal and
-- recursion ends.
--
-- While the leaf nodes are completely undefined in this formulation
-- --- since they primarily exist for frequency estimation and
-- agglomerative methods --- they are arbitrarily defined to hold the
-- whole-data goodness at each final node.

-- The splitters
-- 
-- Splitters can be any data type at all so long as a method for using
-- that object to split a given list is instantiated.
class Splitter a c | a -> c where
    sfilter :: c -> a -> Bool
    splitObs :: [(y, a)] -> c -> ([y], [y])
    splitObs dat spl = let (a, b) = partition (sfilter spl . snd) dat
                       in (map fst a, map fst b)

instance Splitter [[Bool]] (Int, Int) where
    sfilter (i, j) pred = pred !! i !! j

-- Tree growth
-- 
-- This function embodies the growing algorithm as explained above. In
-- particular, it's parametric in question generation (conditional on
-- a path-dependent, type parametric seed), split selection, and
-- sample "goodness" score. Even the goodness comparison function is
-- parameterized since the cutoff might be variable, though it's
-- expected most will be of the form ((> n) . (-))
growTree1 :: (Ord y, Eq y, Splitter x spl) => 
             [(y, x)]                   -- Observations, data `y` predicted by `x`
          -> Float                      -- Hold-out percentage
          -> (seed -> [spl])            -- Split proposal function
          -> (seed -> spl -> seed)      -- Seed update
          -> seed                       -- Initial seed
          -> ([([y], [y])] -> Int)      -- Split selection function
          -> ([y] -> Double)            -- "Goodness" function
          -> (Double -> Double -> Bool) -- "Goodness" comparator, 
                                        -- "old" -> "new" -> "continue growing?"
          -> DTree spl Double
growTree1 dat perc propose update seed0 select score continue = 
    let (dev, ho) = splitAt (ceiling $ (fromIntegral $ length dat) * perc) dat
    in grow' dev ho seed0 (score $ map fst ho)
    where grow' dev ho seed goodness = 
              let splitters = propose seed
                  proposals = map (splitObs dev) splitters
                  bestIdx   = select proposals
                  bestSpl   = splitters !! bestIdx
                  seed'     = update seed bestSpl
                  (devl, devr) = partition (sfilter bestSpl . snd) dev
                  (hol, hor)   = partition (sfilter bestSpl . snd) ho
                  (nl, nr)  = (fromIntegral $ length hol, fromIntegral $ length hor)
                  n         = nl + nr
                  (fl, fr)  = (nl/n, nr/n)
                  goodnessl = score (map fst hol)
                  goodnessr = score (map fst hor)
                  goodness' = fl*goodnessl + fr*goodnessr
              in if continue goodness goodness'
                 then Branch bestSpl (grow' devl hol seed' goodnessl)
                                     (grow' devr hor seed' goodnessr)
                 else Leaf goodness'