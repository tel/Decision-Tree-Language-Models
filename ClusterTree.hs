{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}

module ClusterTree (ClusterTree, singletons, BTree(Leaf, Split)) where

import Text.ParserCombinators.Parsec
import qualified Data.Set as Set

import BTree

type ClusterTree = BTree (Set.Set Char)
instance Show ClusterTree where
  show (Leaf x) = (Set.toList x)
  show (Split l r) = "(" ++ show l ++ " " ++ show r ++ ")"

instance Show (BTree Char) where
  show (Leaf x) = [x]
  show (Split l r) = "(" ++ show l ++ " " ++ show r ++ ")"

-- Get our alphabet
alphabet = "abcdefghijklmnopqrstuvwxyz "
singletons = map (Leaf . Set.singleton) alphabet

parseClusterTree :: Parser ClusterTree
parseClusterTree = parseLeaf <|> parseSplit
  where parseLeaf = do 
          c <- oneOf alphabet
          return $ Leaf (Set.singleton c)
        parseSplit = do
          char '('
          l <- parseClusterTree
          char ' '
          r <- parseClusterTree
          char ')'
          return $ Split l r

class ParsecRead a where
    parsecRead :: Parser a
    
instance ParsecRead ClusterTree where
  parsecRead = parseClusterTree

instance (ParsecRead a) => Read a where
  readsPrec _ = either (const []) id . parse parsecRead' "" 
    where parsecRead' = do a <- parsecRead
                           rest <- getInput
                           return [(a, rest)]
