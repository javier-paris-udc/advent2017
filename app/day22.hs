module Main where

import           AoC                (applyInput)
import qualified Data.Map           as Map
import           Data.Map.Strict    (Map)
import           Data.Maybe         (fromMaybe)
import           Text.Parsec        ((<|>), char, many1, sepEndBy1, spaces)
import           Text.Parsec.String (Parser)

type Coord = (Int, Int)

data Dir = N | S | E | W deriving (Show, Eq)

data NodeSt = Infected | Clean | Weakened | Flagged deriving (Show, Eq)


left :: Dir -> Dir
left d = case d of
    N -> W
    W -> S
    S -> E
    E -> N


right :: Dir -> Dir
right d = case d of
    N -> E
    E -> S
    S -> W
    W -> N


op :: Dir -> Dir
op d = case d of
    N -> S
    S -> N
    W -> E
    E -> W


move :: Dir -> Coord -> Coord
move d (x, y) = case d of
    N -> (x - 1, y)
    S -> (x + 1, y)
    W -> (x, y - 1)
    E -> (x, y + 1)


nodeDir :: NodeSt -> Dir -> Dir
nodeDir n = case n of
    Clean    -> left
    Infected -> right
    Weakened -> id
    Flagged  -> op


step :: Int -> (NodeSt -> NodeSt) -> Coord -> Dir -> Map Coord NodeSt -> Int -> Int
step n nextNodeSt pos dir infectedMap infected
    | n == 0 = infected
    | otherwise =
        let st = fromMaybe Clean $ infectedMap Map.!? pos
        in step
                (n-1)
                nextNodeSt
                (move (nodeDir st dir) pos)
                (nodeDir st dir)
                (Map.insert pos (nextNodeSt st) infectedMap)
                (if nextNodeSt st == Infected then infected + 1 else infected)


solve :: Int -> (NodeSt -> NodeSt) -> (Coord, Map Coord NodeSt) -> Int
solve n nextNodeSt (pos0, infectedM) =  step n nextNodeSt pos0 N infectedM 0


solveP1 :: (Coord, Map Coord NodeSt) -> Int
solveP1 = solve 10_000 nextNodeSt
  where
    nextNodeSt s = case s of
        Infected -> Clean
        Clean -> Infected
        _ -> error "invalid state"


solveP2 :: (Coord, Map Coord NodeSt) -> Int
solveP2 = solve 10_000_000 nextNodeSt
  where
    nextNodeSt s = case s of
        Clean    -> Weakened
        Weakened -> Infected
        Infected -> Flagged
        Flagged  -> Clean


boardP :: Parser (Coord, Map Coord NodeSt)
boardP = do
    rows <- many1 (char '.' <|> char '#') `sepEndBy1` spaces
    let stX = length rows `div` 2
        stY = case rows of
            [] -> error "empty map"
            (l:_) -> length l `div` 2
        set = Map.fromList
            [((x, y), Infected) | (x, r) <- zip [0..] rows, (y, c) <- zip [0..] r, c == '#']
    pure ((stX, stY), set)


main :: IO ()
main = applyInput boardP solveP1 solveP2