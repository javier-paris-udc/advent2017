module Main where

import           AoC             (applyInput, groupsOf, sep)
import           Control.Arrow   ((>>>))
import           Data.Function   ((&))
import           Data.List       (transpose)
import qualified Data.Map.Strict as Map
import           Text.Parsec     ((<|>), many1, sepBy1, sepEndBy1, spaces, string)


type Pattern = [[Bool]]


divide :: Int -> Pattern -> [Pattern]
divide n = map (groupsOf n)
     >>> groupsOf n
     >>> concatMap transpose


allRotations :: Pattern -> [Pattern]
allRotations p =
    [id
    ,map reverse . transpose
    ,map reverse . reverse
    ,reverse . transpose
    ,transpose
    ,reverse
    ,map reverse
    ,reverse . map reverse . transpose
    ] <*> [p]


rulesToMap :: [(Pattern, Pattern)] -> Map.Map Pattern Pattern
rulesToMap = Map.fromList . concatMap (\(from, to) -> map (, to) (allRotations from))


iter :: Int -> Map.Map Pattern Pattern -> Map.Map Pattern Int -> Map.Map Pattern Int
iter 0 _ m = m
iter n rules m = iter (n - 1) rules (Map.foldlWithKey' addKey Map.empty m)
  where
    addKey m0 pat v =
        foldl' (\macc p -> Map.insertWith (+) p v macc) m0 (newPats pat)

    newPats pat = case length pat of
        3 -> [rules Map.! pat]
        4 -> divide 2 pat
           & map (rules Map.!)
           & groupsOf 2
           & concatMap fuse
           & divide 2
        2 -> [rules Map.! pat]
        _ -> error "patters should have a length of 2,3 or 4"
    fuse [l1, l2] = zipWith (++) l1 l2
    fuse _ = error "fusing several lists"


countTrues :: Map.Map Pattern Int -> Int
countTrues = Map.foldlWithKey' (\s pat v -> s + v * trues pat) 0
  where
    trues = length . concatMap (filter id)


pat0 :: Pattern
pat0 = [[False, True, False], [False, False, True], [True, True, True]]


solve :: Int -> [(Pattern, Pattern)] -> Int
solve n pats = countTrues $ iter n (rulesToMap pats) (Map.singleton pat0 1)


main :: IO ()
main = applyInput (ruleP `sepEndBy1` spaces) (solve 5) (solve 18)
  where
    ruleP = liftA2 (,) (lineP `sepBy1` string "/") (sep "=>" >> lineP `sepBy1` string "/")
    lineP = many1 $ (string "#" >> pure True) <|> (string "." >> pure False)
