module Motion where


data Motion = TLeft | TRight deriving (Eq, Ord)
data Path = Path {unPath :: [Motion]}
emptyPath = Path []

instance Show Motion where
  show TLeft = "."
  show TRight = "+"

instance Show Path where
  show (Path st) = "<" ++ concatMap show st ++ ">"
