module Main where

import           AoC                (applyInput, blanksP, intP)
import qualified Data.IntMap.Strict as Map
import qualified Data.Set           as Set
import           Text.Parsec        (sepBy, sepEndBy1, spaces, string)
import           Text.Parsec.String (Parser)



groups :: Int -> Map.IntMap [Int] -> Int
groups n m =
    let set    = connectedTo n Set.empty m
        newMap = Set.foldl' (flip Map.delete) m set
    in case Map.keys newMap of
        [] -> 1
        (k:_) -> 1 + groups k newMap


connectedTo :: Int -> Set.Set Int -> Map.IntMap [Int] -> Set.Set Int
connectedTo n set m
    | n `Set.member` set = set
    | otherwise =
        let conns = m Map.! n
        in foldl' (\s c -> connectedTo c s m) (Set.insert n set) conns


solveP2 :: Map.IntMap [Int] -> Int
solveP2 = groups 0


solveP1 :: Map.IntMap [Int] -> Int
solveP1 = Set.size . connectedTo 0 Set.empty


pipesP :: Parser (Map.IntMap [Int])
pipesP = Map.fromList <$> (pipeP `sepEndBy1` spaces)
  where
    pipeP = do
        n     <- intP
        pipes <- blanksP >> string "<->" >> blanksP >> intP `sepBy` (string "," >> blanksP)
        return (n, pipes)


main :: IO ()
main = applyInput pipesP solveP1 solveP2