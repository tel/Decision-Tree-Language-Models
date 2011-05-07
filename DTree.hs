{-# LANGUAGE 
  FlexibleInstances, 
  OverlappingInstances, 
  TypeSynonymInstances #-}
module DTree 
    ( DTree(..), BTree, bBranch,
      Dir(..),
      flatten, pathMap ) where

-- Alright.
import qualified Data.Foldable as F (Foldable, foldr, foldMap, maximum)
import Data.Traversable (Traversable, traverse)
import Data.Monoid (Monoid)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Text.Read (lift, readPrec)
import Text.ParserCombinators.ReadP
import Data.Map as M

-- The general decision tree type. It's particularly important to note
-- that types can be stored at both branch and leaf nodes (important
-- for storing the questions) although a simpler interface exists as
-- BTree which defaults () to the branch type.
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