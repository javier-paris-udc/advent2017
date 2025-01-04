module Main where

import           AoC        (applyInput, intP)
import qualified Data.Map   as Map
import           Data.Maybe (mapMaybe)

type Coord = (Int, Int)


solveP2 :: Int -> Int
solveP2 n = spiral (0, 1) (Map.singleton (0,0) 1)
  where
    spiral :: Coord -> Map.Map Coord Int -> Int
    spiral (cx, cy) m =
        let neigh = [ (x, y) | x <- [cx - 1 .. cx + 1], y <- [cy - 1 .. cy + 1], (x, y) /= (cx, cy)]
            val = sum $ mapMaybe (m Map.!?) neigh
        in if val > n then val
           else spiral (next m (cx, cy)) (Map.insert (cx, cy) val m)

    next m (x, y)
        | (x, y - 1) `Map.member` m && (x - 1, y) `Map.notMember` m = (x - 1, y)
        | (x, y - 1) `Map.notMember` m && (x + 1, y) `Map.member` m = (x, y - 1)
        | (x + 1, y) `Map.notMember` m && (x, y + 1) `Map.member` m = (x + 1, y)
        | (x, y + 1) `Map.notMember` m && (x - 1, y) `Map.member` m = (x, y + 1)
        | otherwise = undefined


solveP1 :: Int -> Int
solveP1 n
    | n == 1 = 0
    | otherwise = findLevel (1, 1, 1, 0)
  where
    findLevel (base, end, side, level)
        | n >= base && n <= end =
            let pos = (n - base) `mod` (side - 1)
            in abs (pos - ((side - 1) `div` 2 - 1)) + level
        | otherwise = findLevel (end + 1, end + side * 4 + 4, side + 2, level + 1)


main :: IO ()
main = applyInput intP solveP1 solveP2