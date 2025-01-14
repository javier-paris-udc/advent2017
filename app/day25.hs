module Main where

import           AoC                (applyInput1, intP)
import           Data.Map.Strict    (Map)
import qualified Data.Map           as Map
import           Text.Parsec.String (Parser)
import           Data.Maybe         (fromMaybe)
import           Text.Parsec        ((<|>), alphaNum, many1, sepEndBy1, spaces, string, try)

data Move = R | L deriving (Show, Eq)

type Turing = Map String (Map Int (Int, Move, String))


move :: Move -> Int -> Int
move L  = subtract 1
move R = (+1)


runTuring :: Int -> Int -> String -> Turing -> Map Int Int -> Int
runTuring rounds pos stName turing m
    | rounds == 0 = Map.size (Map.filter (==1) m)
    | otherwise =
        let val                  = fromMaybe 0 (m Map.!? pos)
            (write, dir, nextSt) = turing Map.! stName Map.! val
        in runTuring (rounds - 1) (move dir pos) nextSt turing (Map.insert pos write m)


solve :: (String, Int, Turing) -> Int
solve (iniSt, check, turing) = runTuring check 0 iniSt turing Map.empty


turingP :: Parser (String, Int, Turing)
turingP = do
    iniSt  <- string "Begin in state " >> name <* string "." <* spaces
    check  <- string "Perform a diagnostic checksum after " >> intP <* string " steps." <* spaces
    states <- stateP `sepEndBy1` spaces
    return (iniSt, check, Map.fromList states)
  where
    name = many1 alphaNum

    stateP = do
        st <- string "In state " >> name <* string ":" <* spaces
        rules <- ruleP `sepEndBy1` spaces
        return (st, Map.fromList rules)

    ruleP = do
        val   <- try $ string "If the current value is " >> intP <* string ":" <* spaces
        write <- string "- Write the value " >> intP <* string "." <* spaces
        mov   <- string "- Move one slot to the " >> moveP <* string "." <* spaces
        next  <- string "- Continue with state " >> name <* string "."
        return (val, (write, mov, next))

    moveP = (string "left" >> return L) <|> (string "right" >> return R)


main :: IO ()
main = applyInput1 turingP solve