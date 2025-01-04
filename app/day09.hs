module Main where

import AoC                (applyInput)
import Text.Parsec        ((<|>), anyChar, choice, many, noneOf, sepBy, string, try)
import Text.Parsec.String (Parser)


data Group = Group [Group] | Garbage Int deriving (Show, Eq)


score :: Int -> Group -> Int
score level (Group l) = level + sum (map (score (level +1)) l)
score _ (Garbage _)   = 0


solveP2 :: Group -> Int
solveP2 (Group l)   = sum $ map solveP2 l
solveP2 (Garbage n) = n


solveP1 :: Group -> Int
solveP1 = score 1


groupP :: Parser Group
groupP = do
    contents <- string "{" >> (contentP `sepBy` string ",") <* string "}"
    return $ Group contents
  where
    contentP = try groupP <|> try (Garbage . sum <$> garbageP)
    garbageP = try $ string "<" >> many garbageContent <* string ">"
    garbageContent = choice [string "!" >> anyChar >> pure 0
                            ,noneOf ">" >> pure 1
                            ]


main :: IO ()
main = applyInput groupP solveP1 solveP2