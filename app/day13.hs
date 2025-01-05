module Main where

import AoC         (applyInput, blanksP, intP)
import Text.Parsec (sepEndBy1, spaces, string)


suppress :: [Int] -> (Int, Int) -> [Int]
suppress l (depth, range) = filter (\n -> (n + depth) `mod` (2 * (range - 1)) /= 0) l


solveP2 :: [(Int, Int)] -> Int
solveP2 scanners = case foldl' suppress [0..] scanners of
    [] -> undefined -- infinite list, cannot happen
    (t:_) -> t


solveP1 :: [(Int, Int)] -> Int
solveP1 = sum . map (uncurry (*)). filter (\(depth, range) -> depth `mod` (2 * (range - 1)) == 0)


main :: IO ()
main = applyInput (scannerP `sepEndBy1` spaces) solveP1 solveP2
  where
    scannerP = liftA2 (,) (intP <* string ":" <* blanksP) intP