{-# LANGUAGE RecordWildCards #-}
module Main where

import           AoC                 (applyInput, intP, fst3, snd3, thd3)
import           Text.Parsec         (sepEndBy1, string, spaces)
import           Text.Parsec.String  (Parser)
import           Control.Applicative (liftA3)
import           Data.Foldable       (minimumBy)
import           Control.Arrow       ((>>>))
import           Data.Function       (on)
import qualified Data.Set            as Set

type Coord = (Int, Int, Int)
data Particle = Particle { pos :: Coord, vel :: Coord, accel :: Coord } deriving (Show, Eq, Ord)


totalAccel :: Particle -> Int
totalAccel part = let (ax, ay, az) = accel part in abs ax + abs ay + abs az


sumCoords :: Coord -> Coord -> Coord
sumCoords (x, y, z) (a, b, c) = (x + a, y +b, z+ c)


move :: Particle -> Particle
move p = p { pos = newPos, vel = newVel }
  where
    newVel = sumCoords (vel p) (accel p)
    newPos = sumCoords (pos p) newVel


isReachable :: Set.Set Particle -> Particle -> Bool
isReachable parts p = any (\p2 -> p /= p2 && all (canReach p2 p) [fst3, snd3, thd3]) parts
  where
    canReach p1 p2 coordF =
        not (coordF (pos p1) > coordF (pos p2)
          && coordF (vel p1) >= coordF (vel p2)
          && coordF (accel p1) >= coordF (accel p2))
     && not (coordF (pos p1) < coordF (pos p2)
          && coordF (vel p1) <= coordF (vel p2)
          && coordF (accel p1) <= coordF (accel p2))


simulate :: Set.Set Particle -> Int
simulate parts
    | null parts = 0
    | otherwise  = Set.size nonReachable + simulate reachable
  where
    surviving = Set.filter (\p -> all (\p2 -> p == p2 || pos p /= pos p2) parts) parts

    steppedParts = Set.map move surviving

    (reachable, nonReachable) = Set.partition (isReachable steppedParts) steppedParts



solveP2 :: [Particle] -> Int
solveP2 = simulate . Set.fromList


solveP1 :: [Particle] -> Int
solveP1 = zip [0..]
      >>> minimumBy (compare `on` (totalAccel . snd))
      >>> fst


particleP :: Parser Particle
particleP = do
    pos   <- coordP "p"
    vel   <- string ", " *> coordP "v"
    accel <- string ", " *> coordP "a"
    pure $ Particle { .. }
  where
    coordP str = do
        _ <- string str >> string "="
        liftA3 (,,) (string "<" *> intP) (string "," *> intP) (string "," *> intP <* string ">")


main :: IO ()
main = applyInput (particleP `sepEndBy1` spaces) solveP1 solveP2