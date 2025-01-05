module Main where

import AoC            (applyInput, intP)
import Control.Arrow  ((>>>))
import Data.Bifunctor (bimap)
import Data.Bits      ((.&.))
import Text.Parsec    (spaces, string)


generate :: Int -> Int -> Int
generate factor val = factor * val `mod` 2147483647

fa, fb :: Int -> Int
fa = generate 16807
fb = generate 48271


solve :: Int -> ([Int], [Int]) -> Int
solve n = bimap (take 40_000_000) (take n)
    >>> bimap (map lowest16) (map lowest16)
    >>> uncurry (zipWith (==))
    >>> filter id
    >>> length
  where
    lowest16 = (.&.) (2 ^ (16 :: Int) - 1)


solveP2 :: (Int, Int) -> Int
solveP2 = bimap (drop 1 . iterate fa) (drop 1 . iterate fb)
      >>> bimap (filter ((==0) . (`mod` 4))) (filter ((==0) . (`mod` 8)))
      >>> solve 5_000_000


solveP1 :: (Int, Int) -> Int
solveP1 = bimap (drop 1 . iterate fa) (drop 1 . iterate fb)
      >>> solve 40_000_000


main :: IO ()
main = applyInput seedsP solveP1 solveP2
  where
    seedsP = liftA2 (,) (string "Generator A starts with " >> intP <* spaces)
                        (string "Generator B starts with " >> intP)
