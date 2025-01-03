module Main where

import AoC           (applyInput)
import Data.Function ((&))
import Text.Parsec   (digit, many1)


solveP2 :: [Int] -> Int
solveP2 n = zip [0..] n
          & filter (\(i, d) -> n !! ((i + half) `mod` len) == d)
          & map snd
          & sum
  where
    len = length n
    half = len `div` 2


solveP1 :: [Int] -> Int
solveP1 n = case n of
    [] -> 0
    (d:ds) -> zip n (ds ++ [d])
            & filter (uncurry (==))
            & map fst
            & sum


main :: IO ()
main = applyInput (many1 (read . (:[]) <$> digit)) solveP1 solveP2