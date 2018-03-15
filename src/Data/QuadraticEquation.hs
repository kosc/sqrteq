module Data.QuadraticEquation
  ( QuadraticEquation (..)
  , Solution (..)
  , discriminant
  , solve
  ) where

import           Data.Monoid ((<>))

data QuadraticEquation a = QuadraticEquation a a a

data Solution a
    = NoSolutions
    | OneSolution a
    | TwoSolutions a a

instance Show a => Show (Solution a) where
    show  NoSolutions         = "There are no solutions"
    show (OneSolution x)      = "There is only one solution: " <> show x
    show (TwoSolutions x1 x2) = "Solutions: " <> show x1 <> ", " <> show 2

discriminant :: Floating a => QuadraticEquation a -> a
discriminant (QuadraticEquation a b c) = (b * b) - (4 * a * c)

solve :: (Floating a, Ord a) => QuadraticEquation a -> Solution a
solve    (QuadraticEquation 0 b c) = OneSolution (-c / b)
solve eq@(QuadraticEquation a b c) = case compare discriminant' 0 of
    LT -> NoSolutions
    EQ -> OneSolution x1
    GT -> TwoSolutions x1 x2
  where
    discriminant' = discriminant eq
    center        = -b / (2 * a)
    offset        = sqrt discriminant' / (2 * a)
    x1            = center + offset
    x2            = center - offset
    alone         = -c / b
