module Shuffle (shuffle) where

import System.Random
import Data.Map hiding (foldl)

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

-- | Da una permutación aleatoria de una lista.
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l@(x:xs) = toElems $ foldl fisherYatesStep (initial x gen) (numerate xs)
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen) 
