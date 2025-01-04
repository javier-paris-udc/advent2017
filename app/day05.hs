module Main where

import           AoC         (applyInput, intP)
import qualified Data.IntMap as Map
import           Text.Parsec (sepEndBy1, spaces)


follow :: (Int -> Int) -> Int -> Int -> Map.IntMap Int -> Int
follow modify steps ip m =
    case m Map.!? ip of
        Nothing -> steps
        Just rel -> follow modify (steps + 1) (ip + rel) (Map.insert ip (modify rel) m)


solveP2 :: [Int] -> Int
solveP2 = follow modify 0 0 . Map.fromList . zip [0..]
  where
    modify n
        | n >= 3 = n - 1
        | otherwise = n + 1


solveP1 :: [Int] -> Int
solveP1 = follow (+1) 0 0 . Map.fromList . zip [0..]



main :: IO ()
main = applyInput (intP `sepEndBy1` spaces) solveP1 solveP2