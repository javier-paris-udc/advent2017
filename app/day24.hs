module Main where

import           AoC                (applyInput, intP)
import           Data.Bifunctor     (bimap)
import           Data.Function      ((&))
import qualified Data.IntSet        as Set
import           Data.IntSet        (IntSet)
import qualified Data.IntMap        as Map
import           Data.IntMap.Strict (IntMap)
import           Text.Parsec        (sepEndBy1, spaces, string)


removePorts :: Int -> Int -> IntMap IntSet -> IntMap IntSet
removePorts p v ports =
      Map.alter (remove v) p ports
    & Map.alter (remove p) v
  where
    remove _ Nothing = Nothing
    remove x (Just set) =
        let newSet = Set.delete x set in
        if Set.null newSet then Nothing
        else Just newSet


strongest :: Int -> IntMap IntSet -> Int
strongest p ports = case ports Map.!? p of
    Nothing  -> 0
    Just set -> Set.findMax (Set.map (\v -> p + v + strongest v (removePorts p v ports)) set)


longest :: Int -> IntMap IntSet -> (Int, Int)
longest p ports = case ports Map.!? p of
    Nothing -> (0, 0)
    Just set -> Set.toList set
              & map (\v -> bimap (+1) (+ (p + v)) $ longest v (removePorts p v ports))
              & maximum


buildMap :: [(Int, Int)] -> IntMap IntSet
buildMap = foldl' addPair Map.empty . sortPairs
  where
    addPair m (a, b) =
        Map.insertWith Set.union a (Set.singleton b) m
      & Map.insertWith Set.union b (Set.singleton a)

    sortPairs = map (\(a, b) -> (min a b, max a b))


solveP2 :: [(Int, Int)] -> Int
solveP2 = snd . longest 0 . buildMap


solveP1 :: [(Int, Int)] -> Int
solveP1 = strongest 0 . buildMap


main :: IO ()
main = applyInput (portP `sepEndBy1` spaces) solveP1 solveP2
  where
    portP = liftA2 (,) intP (string "/" >> intP)