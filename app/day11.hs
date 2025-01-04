module Main where

import           AoC                (applyInput)
import           Data.List          (scanl')
import qualified Data.Map.Strict    as Map
import           Text.Parsec        (choice, sepBy, string, try)
import           Text.Parsec.String (Parser)


data Dir = NE | NW | N | SE | SW | S deriving (Show, Eq, Ord)


opposite :: Dir -> Dir
opposite d = case d of
    N  -> S
    S  -> N
    NE -> SW
    SW -> NE
    NW -> SE
    SE -> NW


simplifications :: Dir -> ((Dir, Dir), (Dir, Dir))
simplifications d = case d of
    N  -> ((SE, NE), (SW, NW))
    S  -> ((NE, SE), (NW, SW))
    NE -> ((S , SE), (NW, N) )
    NW -> ((S , SW), (NE, N) )
    SE -> ((N , NE), (SW, S) )
    SW -> ((N,  NW), (SE, S) )


doSolve :: Map.Map Dir Int -> Dir -> Map.Map Dir Int
doSolve m d
    | op > 0     = Map.adjust (subtract 1) (opposite d) m
    | s1Seen > 0 = Map.adjust (+1) ss1 $ Map.adjust (subtract 1) s1 m
    | s2Seen > 0 = Map.adjust (+1) ss2 $ Map.adjust (subtract 1) s2 m
    | otherwise  = Map.adjust (+1) d m
  where
    op = m Map.! opposite d
    ((s1, ss1), (s2, ss2)) = simplifications d
    s1Seen = m Map.! s1
    s2Seen = m Map.! s2


emptyMap :: Map.Map Dir Int
emptyMap = Map.fromList [(N, 0), (S, 0), (NE, 0), (NW, 0), (SE, 0), (SW, 0)]


solveP2 :: [Dir] -> Int
solveP2 = maximum . map sum . scanl' doSolve emptyMap


solveP1 :: [Dir] -> Int
solveP1 = sum . foldl' doSolve emptyMap


dirsP :: Parser [Dir]
dirsP = dirP `sepBy` string ","
  where
    dirP = choice [try $ string "ne" >> pure NE
                  ,try $ string "nw" >> pure NW
                  ,try $ string "n"  >> pure N
                  ,try $ string "se" >> pure SE
                  ,try $ string "sw" >> pure SW
                  ,try $ string "s"  >> pure S]


main :: IO ()
main = applyInput dirsP solveP1 solveP2