module Main where

import AoC                (applyInputSWith)
import Data.Array         (Array, Ix (inRange), (!), array, indices, bounds)
import Text.Parsec        (char, choice, letter, many1, newline, sepEndBy1)
import Text.Parsec.String (Parser)


type Coord = (Int, Int)
data Tile = Empty | Vert | Hor | Cross | Letter Char deriving (Show, Eq)
type Maze = Array Coord Tile
data Dir  = N | S | E | W deriving (Show, Eq)


move :: Dir -> Coord -> Coord
move N (x, y) = (x - 1, y)
move S (x, y) = (x + 1, y)
move E (x, y) = (x, y + 1)
move W (x, y) = (x, y - 1)


switch :: Dir -> [Dir]
switch N = [E, W]
switch S = [E, W]
switch E = [N, S]
switch W = [N, S]


explore :: Int -> String -> Coord -> Dir -> (Coord -> Tile) -> (String, Int)
explore steps str pos dir maze =
    case maze pos of
        Empty    -> (reverse str, steps)
        Vert     -> explore (steps + 1)    str  (move dir pos) dir maze
        Hor      -> explore (steps + 1)    str  (move dir pos) dir maze
        Letter l -> explore (steps + 1) (l:str) (move dir pos) dir maze
        Cross    ->
            case filter ((/= Empty) . maze . (`move` pos)) (switch dir) of
                [newDir] -> explore (steps + 1) str (move newDir pos) newDir maze
                _ -> undefined


getTile :: Maze -> Coord -> Tile
getTile maze pos
    | bounds maze `inRange` pos = maze ! pos
    | otherwise = Empty


solve :: Maze -> (String, Int)
solve maze = explore 0 [] start S (getTile maze)
  where
    start = case filter (\(x, y) -> x == 0 && maze ! (x, y) == Vert) $ indices maze of
        [c] -> c
        _   -> undefined


mazeP :: Parser Maze
mazeP = do
    rows <- many1 tileP `sepEndBy1` newline
    let indexed = [((x, y), t) | (x, r) <- zip [0..] rows, (y, t) <- zip [0..] r]
        maxX = maximum $ map (fst . fst) indexed
        maxY = maximum $ map (snd . fst) indexed
    return $ array ((0, 0), (maxX, maxY)) indexed
  where
    tileP = choice [char ' ' >> pure Empty
                   ,char '|' >> pure Vert
                   ,char '-' >> pure Hor
                   ,char '+' >> pure Cross
                   ,Letter <$> letter
                   ]


main :: IO ()
main = applyInputSWith mazeP () solveAndPrint
  where
    solveAndPrint maze = do
        let (str, steps) = solve maze
        putStrLn str
        print steps