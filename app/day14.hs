module Main where

import           AoC           (applyInputSWith, groupsOf)
import           Data.Bits     (Bits (testBit), popCount, xor)
import           Data.Char     (ord)
import           Data.Foldable (toList)
import           Data.Function ((&))
import           Text.Parsec   (alphaNum, many1)
import qualified Data.Set      as Set
import           Data.Sequence ((><))
import qualified Data.Sequence as Seq
import           Text.Printf   (printf)


n :: Int
n = 256


tie :: (Int, Int, Seq.Seq Int) -> Int -> (Int, Int, Seq.Seq Int)
tie (desp, skip, sequ) len =
    let (st, rest0) = Seq.splitAt len sequ
        (sk, rest1) = Seq.splitAt skip (rest0 >< Seq.reverse st)
    in (desp + skip + len, (skip + 1) `mod` n, rest1 >< sk)


knotHash :: String -> String
knotHash ns = concatMap (printf "%02x" . foldl1 xor) $ groupsOf 16 $ toList finalSeq
  where
    seq0         = Seq.fromList [0 .. (n-1)]
    lengths      = (++ [17, 31, 73, 47, 23]) $ map ord ns
    (desp, _, s) = foldl' tie (0, 0, seq0) (concat $ replicate 64 lengths)
    (st, end)    = Seq.splitAt (n - desp `mod` n) s
    finalSeq     = end >< st


hashes :: String -> [String]
hashes keyStr = map (\(i :: Int) -> knotHash (keyStr ++ ('-':show i))) [0 .. 127]


removeNeighborgs :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
removeNeighborgs s c@(x, y)
    | c `Set.notMember` s = s
    | otherwise = foldl' removeNeighborgs (Set.delete c s) neighbors
  where
    neighbors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]


countGroups :: Set.Set (Int, Int) -> Int
countGroups s = case Set.toList s of
        c:_ -> 1 + countGroups (removeNeighborgs s c)
        []  -> 0


solveP2 :: [Integer] -> Int
solveP2 rows = countGroups (Set.fromList setBits)
  where
    setBits = map (\i -> map (testBit i) [127,126 .. 0]) rows
            & zipWith (\(i :: Int) r -> zipWith (\(j :: Int) b -> ((i,j), b)) [0 ..] r) [0 ..]
            & concatMap (filter snd)
            & map fst

solveP1 :: [Integer] -> Int
solveP1 = sum . map popCount


main :: IO ()
main = applyInputSWith (many1 alphaNum) () solve
  where
    solve str = do
        let rows = map ((read :: String -> Integer) . ("0x" ++)) $ hashes str
        print $ solveP1 rows
        print $ solveP2 rows