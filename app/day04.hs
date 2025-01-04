module Main where

import AoC                (applyInput, blanksP)
import Data.List          (group, sort)
import Text.Parsec        (letter, many1, sepEndBy1, spaces)
import Text.Parsec.String (Parser)


valid :: [String] -> Bool
valid pass = length (group (sort pass)) == length pass


solveP2 :: [[String]] -> Int
solveP2 = solveP1 . map (map sort)


solveP1 :: [[String]] -> Int
solveP1 = length . filter valid


passP :: Parser [[String]]
passP = (many1 letter `sepEndBy1` blanksP) `sepEndBy1` spaces


main :: IO ()
main = applyInput passP solveP1 solveP2