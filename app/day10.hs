module Main where

import           AoC           (applyInput, groupsOf, intP)
import           Data.Bits     (xor)
import           Data.Char     (ord)
import           Data.Foldable (toList)
import           Data.List     (intercalate)
import           Data.Sequence ((><))
import qualified Data.Sequence as Seq
import           Text.Parsec   (sepBy1, string)
import           Text.Printf   (printf)


n :: Int
n = 256


tie :: (Int, Int, Seq.Seq Int) -> Int -> (Int, Int, Seq.Seq Int)
tie (desp, skip, sequ) len =
    let (st, rest0) = Seq.splitAt len sequ
        (sk, rest1) = Seq.splitAt skip (rest0 >< Seq.reverse st)
    in (desp + skip + len, (skip + 1) `mod` n, rest1 >< sk)


solveP2 :: [Int] -> String
solveP2 ns = concatMap (printf "%02x" . foldl1 xor) $ groupsOf 16 $ toList finalSeq
  where
    seq0         = Seq.fromList [0 .. (n-1)]
    lengths      = (++ [17, 31, 73, 47, 23]) $ map ord $ intercalate "," $ map show ns
    (desp, _, s) = foldl' tie (0, 0, seq0) (concat $ replicate 64 lengths)
    (st, end)    = Seq.splitAt (n - desp `mod` n) s
    finalSeq     = end >< st


solveP1 :: [Int] -> Int
solveP1 lengths = product $ Seq.take 2 $ end >< st
  where
    seq0         = Seq.fromList [0 .. (n-1)]
    (desp, _, s) = foldl' tie (0, 0, seq0) lengths
    (st, end)    = Seq.splitAt (n - desp `mod` n) s


main :: IO ()
main = applyInput (intP `sepBy1` string ",") solveP1 solveP2