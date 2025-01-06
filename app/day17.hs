module Main where

import           AoC           (applyInput, intP)
import qualified Data.Sequence as Seq


buffer :: Int -> Int -> Int -> Seq.Seq Int -> Int
buffer skip n pos sq
    | n == 2018 = Seq.index sq ((pos - skip) `mod` n)
    | otherwise =
        let nextPos0 = (pos + skip + 1) `mod` (n + 1)
            nextPos  = if nextPos0 == 0 then n + 1 else nextPos0
        in buffer skip (n + 1) nextPos (Seq.insertAt pos n sq)


follow0 :: Int -> Int -> Int -> Int -> Int
follow0 skip n pos after0
    | n == 50_000_000 = after0
    | pos == 1  = follow0 skip (n + 1) nextPos n
    | otherwise = follow0 skip (n + 1) nextPos after0
  where
    nextPos0 = (pos + skip + 1) `mod` (n + 1)
    nextPos  = if nextPos0 == 0 then n + 1 else nextPos0


solveP2 :: Int -> Int
solveP2 skip = follow0 skip 1 1 0


solveP1 :: Int -> Int
solveP1 skip = buffer skip 1 1 (Seq.singleton 0)


main :: IO ()
main = applyInput intP solveP1 solveP2