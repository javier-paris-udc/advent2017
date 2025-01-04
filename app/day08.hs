{-# LANGUAGE RecordWildCards #-}
module Main where

import           AoC                (applyInput, blanksP, intP)
import           Data.List          (scanl')
import qualified Data.Map.Strict    as Map
import           Text.Parsec        ((<|>), alphaNum, choice, many1, sepEndBy1, spaces, string, try)
import           Text.Parsec.String (Parser)


data Cmp = Gt | Lt | Ge | Le | Eql | Neq
    deriving (Show, Eq)
data Inst = Inst { reg :: String, diff :: Int, regC :: String, cmp :: Cmp, cmpVal :: Int}
    deriving (Show, Eq)


cmpToFun :: Ord a => Cmp -> a -> a -> Bool
cmpToFun c = case c of
    Gt  -> (>)
    Lt  -> (<)
    Ge  -> (>=)
    Le  -> (<=)
    Eql -> (==)
    Neq -> (/=)


runInst :: Map.Map String Int -> Inst -> Map.Map String Int
runInst m i
    | cmpToFun (cmp i) (Map.findWithDefault 0 (regC i) m) (cmpVal i) =
        Map.insertWith (+) (reg i) (diff i) m
    | otherwise = m


solveP2 :: [Inst] -> Int
solveP2 = maximum . map maximum . filter (not . null) . scanl' runInst Map.empty


solveP1 :: [Inst] -> Int
solveP1 = maximum . foldl' runInst Map.empty


progP :: Parser [Inst]
progP = instP `sepEndBy1` spaces
  where
    instP = do
        reg <- many1 alphaNum
        sign <- blanksP >> ((string "inc" >> pure 1) <|> (string "dec" >> pure (-1)))
        diff <- blanksP >> ((sign *) <$> intP)
        regC <- blanksP >> string "if" >> blanksP >> many1 alphaNum
        cmp <- blanksP >> cmpP
        cmpVal <- blanksP >> intP
        return $ Inst { .. }

    cmpP = choice [try $ string ">=" >> pure Ge
                  ,try $ string "<=" >> pure Le
                  ,try $ string ">"  >> pure Gt
                  ,try $ string "<"  >> pure Lt
                  ,try $ string "==" >> pure Eql
                  ,try $ string "!=" >> pure Neq
                  ]


main :: IO ()
main = applyInput progP solveP1 solveP2