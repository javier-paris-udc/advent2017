module Main where

import AoC                (applyInput, blanksP, intP)
import Control.Arrow      ((&&&))
import Text.Parsec        (sepEndBy1, spaces)
import Text.Parsec.String (Parser)


pairs :: Eq a => [a] -> [(a, a)]
pairs l = [(a, b) | a <- l, b <- l, a /= b]


solveP2 :: [[Int]] -> Int
solveP2 = sum . concatMap (map (uncurry div) . filter (\(n1, n2) -> n1 `mod` n2 == 0) . pairs)


solveP1 :: [[Int]] -> Int
solveP1 = sum . map (uncurry (-) . (maximum &&& minimum))


spreadP :: Parser [[Int]]
spreadP = rowP `sepEndBy1` spaces
  where
    rowP = intP `sepEndBy1` blanksP


main :: IO ()
main = applyInput spreadP solveP1 solveP2