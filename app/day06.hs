module Main where

import           AoC           (applyInput, intP)
import           Data.Foldable (maximumBy)
import qualified Data.Map      as Map
import           Text.Parsec   (sepEndBy1, spaces)


redistribute :: [Int] -> Int -> Int -> Int -> [Int]
redistribute nums val size p = zipWith getVal [0 .. ] nums
  where
    addToAll = val `div` size
    left = val `mod` size

    getVal i v
        | i == p = addToAll
        | p + left < size && i > p && i <= p + left = v + addToAll + 1
        | p + left >= size && ((i > p) || i <= p + left - size) = v + addToAll + 1
        | otherwise = v + addToAll


findLoop :: [Int] -> Int -> Map.Map [Int] Int -> ([Int], Map.Map [Int] Int)
findLoop nums size seen
    | nums `Map.member` seen = (nums, seen)
    | otherwise =
        let (pos, val) = maximumBy valAndPos (zip [0..] nums)
        in findLoop (redistribute nums val size pos) size (Map.insert nums (Map.size seen) seen)
  where
    valAndPos (p1, v1) (p2, v2) =
        case compare v1 v2 of
            EQ  -> compare p2 p1
            cmp -> cmp


solveP2 :: [Int] -> Int
solveP2 nums = let (rep, m) = findLoop nums (length nums) Map.empty
            in Map.size m - (m Map.! rep)

solveP1 :: [Int] -> Int
solveP1 nums = Map.size $ snd $ findLoop nums (length nums) Map.empty


main :: IO ()
main = applyInput (intP `sepEndBy1` spaces) solveP1 solveP2