module Main where

import           AoC                (applyInput, blanksP, intP)
import qualified Data.Map.Strict    as Map
import           Text.Parsec.String (Parser)
import           Text.Parsec        (between, letter, many1, option, sepBy1, sepEndBy1, spaces, string)
import           Data.List          (findIndex)

type PMap = Map.Map String (Int, [String])


weight :: PMap -> String -> Int
weight m key =
    let (w, subkeys) = m Map.! key
    in foldl' (\acc k -> acc + weight m k) w subkeys

reWeight :: PMap -> PMap
reWeight m = Map.map (\(w, subkeys) -> (w + sum (map (weight m) subkeys), subkeys)) m


correctWeight :: String -> Int -> PMap -> PMap -> Int
correctWeight key expected wm m=
    let (w, subkeys) = m Map.! key
        subWeights = map (\k ->fst $ wm Map.! k) subkeys
        expectedSubWeight = (expected - w) `div` length subWeights
    in case subWeights of
        [] -> expected
        w1:ws ->
            if all (==w1) ws then expected - sum subWeights
            else case findIndex (/=expectedSubWeight) subWeights of
                    Just wrongIdx -> correctWeight (subkeys !! wrongIdx) expectedSubWeight wm m
                    Nothing -> undefined


solveP2 :: PMap -> Int
solveP2 m =
    let (_, subkeys) = wm Map.! root
        subWeights = map (\k -> fst $ wm Map.! k) subkeys
    in case subWeights of
        [] -> undefined
        w1:ws ->
            case filter ((/= w1) . snd) (zip [0..] ws) of
                [(k, _)] -> correctWeight (subkeys !! (k + 1)) w1 wm m
                _        ->
                    case ws of
                        []   -> undefined
                        w2:_ -> correctWeight (subkeys !! 0) w2 wm m
  where
    root = solveP1 m
    wm   = reWeight m


solveP1 :: PMap -> String
solveP1 m = case Map.keys unsupportedProgs of
    [k] -> k
    _   -> undefined
  where
    allV             = concatMap snd $ Map.elems m
    unsupportedProgs = foldl' (flip Map.delete) m allV


programsP :: Parser PMap
programsP = do
    progs <- programP `sepEndBy1` spaces
    return $ Map.fromList progs
  where
    programP = do
        name <- many1 letter <* spaces
        weig <- between (string "(") (string ")") intP
        supports <- option [] (blanksP >> string "->" >> blanksP >> (many1 letter `sepBy1` string ", "))
        return (name, (weig, supports))


main :: IO ()
main = applyInput programsP solveP1 solveP2