module Main where

import           AoC                (applyInput, intP)
import qualified Data.Map.Strict    as Map
import           Text.Parsec        (choice, letter, sepBy1, string)
import           Text.Parsec.String (Parser)


data Move = Spin Int | Exchange Int Int | Partner Char Char deriving (Show, Eq)
type State = (Int, Map.Map Char Int, Map.Map Int Char)


size :: Int
size = 16

lm0 :: Map.Map Char Int
lm0 = Map.fromList (zip ['a' .. 'p'] [0 .. size - 1])

nm0 :: Map.Map Int Char
nm0 = Map.fromList (zip [0 .. size - 1] ['a' .. 'p'])


doMove :: State -> Move -> State
doMove (desp, lm, nm) m = case m of
    Spin x         -> ((desp - x) `mod` size, lm, nm)
    Exchange p1 p2 ->
        let despP1 = (p1 + desp) `mod` size
            despP2 = (p2 + desp) `mod` size
            letterP1 = nm Map.! despP1
            letterP2 = nm Map.! despP2
            newNm = Map.insert despP1 letterP2 $ Map.insert despP2 letterP1 nm
            newLm = Map.insert letterP1 despP2 $ Map.insert letterP2 despP1 lm
        in (desp, newLm, newNm)
    Partner letter1 letter2 ->
        let p1 = lm Map.! letter1
            p2 = lm Map.! letter2
            newNm = Map.insert p2 letter1 $ Map.insert p1 letter2 nm
            newLm = Map.insert letter1 p2 $ Map.insert letter2 p1 lm
        in (desp, newLm, newNm)


mapToStr :: State -> String
mapToStr (desp, _, nm) = map ((nm Map.!) . (`mod` size) . (+ desp)) [0 .. size - 1]


findRepeat :: Int -> State -> Map.Map (Int, String) Int -> [(Int, Move)] -> String
findRepeat _ _ _ [] = undefined
findRepeat n st visited ((pos, m):ms)
    | (pos, str) `Map.member` visited =
        let loopSt = visited Map.! (pos, str)
            offset = 1_000_000_000 - loopSt
            cycleLen = n - loopSt
            mapPos = offset `mod` cycleLen
        in case Map.keys (Map.filter (==mapPos) visited) of
            [(_, bStr)] -> bStr
            _ -> undefined
    | otherwise = findRepeat (n + 1) (doMove st m) (Map.insert (pos, str) n visited) ms
  where
    str = mapToStr st


solveP2 :: [Move] -> String
solveP2 = findRepeat 0 (0, lm0, nm0) Map.empty . cycle . zip [0..]


solveP1 :: [Move] -> String
solveP1 = mapToStr . foldl' doMove (0, lm0, nm0)


movesP :: Parser [Move]
movesP = moveP `sepBy1` string ","
  where
    moveP = choice [string "s" >> fmap Spin intP
                   ,string "x" >> liftA2 Exchange intP (string "/" >> intP)
                   ,string "p" >> liftA2 Partner letter (string "/" >> letter)
                   ]


main :: IO ()
main = applyInput movesP solveP1 solveP2