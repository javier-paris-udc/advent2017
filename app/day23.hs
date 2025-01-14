module Main where

import           AoC                              (applyInput, intP)
import           Control.Monad.Trans.State.Strict (State, execState, gets, modify)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Text.Parsec                      (choice, letter, sepEndBy1, spaces, string, try)
import           Text.Parsec.String               (Parser)


data Ref = Reg Char | Val Int deriving (Show, Eq)
data Inst = Set Char Ref
          | Sub Char Ref
          | Mul Char Ref
          | Jnz Ref Ref
    deriving (Show, Eq)

type Prog = Map.Map Int Inst

data St = St {regs :: Map.Map Char Int
             ,muls :: Int
             ,ip :: Int
             } deriving (Show, Eq)


getReg :: Char -> State St Int
getReg r = gets (fromMaybe 0 . (Map.!? r) . regs)


getRef :: Ref -> State St Int
getRef ref = case ref of
    Val x -> pure x
    Reg r -> getReg r


biOp :: (Int -> Int -> Int) -> Char -> Ref -> State St ()
biOp op reg ref = do 
    valReg <- getReg reg
    valRef <- getRef ref
    modify (\st -> st { regs = Map.insert reg (valReg `op` valRef) (regs st), ip = ip st + 1})


setI :: Char -> Ref -> State St ()
setI = biOp (\_ x -> x)


subI :: Char -> Ref -> State St ()
subI = biOp (-)


mulI :: Char -> Ref -> State St ()
mulI reg ref = do
    biOp (*) reg ref
    modify (\st -> let newMuls = muls st + 1 in seq newMuls st { muls = newMuls })


jnzI :: Ref -> Ref -> State St ()
jnzI ref1 ref2 = do
    valRef1 <- getRef ref1
    valRef2 <- getRef ref2
    if valRef1 /= 0 then modify (\st -> st { ip = ip st + valRef2 })
    else modify (\st -> st { ip = ip st + 1 })


doInst :: Prog -> State St ()
doInst prog = do
    instIdx <- gets ip
    case prog Map.! instIdx of
        Set reg ref   -> setI reg ref
        Sub reg ref   -> subI reg ref
        Mul reg ref   -> mulI reg ref
        Jnz ref1 ref2 -> jnzI ref1 ref2


untilM_ :: State St () -> State St Bool -> State St ()
untilM_ st check = do
    st
    end <- check
    if end then pure ()
    else untilM_ st check


isOut :: Prog -> State St Bool
isOut prog = gets (\st -> ip st `Map.notMember` prog)


isPrime :: Int -> Bool
isPrime n
    | n >= 2 && n <= 3 = True
    | even n          = False
    | otherwise       = check 3
  where
    check x
        | x * x > n = True
        | n `mod` x == 0 = False
        | otherwise = check (x + 2)


solveP2 :: Prog -> Int
solveP2 _ =
    length $ filter (not . isPrime) [105700, 105717 .. 122700]


solveP1 :: Prog -> Int
solveP1 prog = muls $ execState (untilM_ (doInst prog) (isOut prog)) st0
  where
    st0 = St { regs = Map.empty, muls = 0, ip = 0 }


progP :: Parser Prog
progP = do
    insts <- instP `sepEndBy1` spaces
    return $ Map.fromList $ zip [0 ..] insts
  where
    instP = choice [try (liftA2 Set (string "set" >> spaces >> letter) (spaces >> refP))
                   ,try (liftA2 Sub (string "sub" >> spaces >> letter) (spaces >> refP))
                   ,try (liftA2 Mul (string "mul" >> spaces >> letter) (spaces >> refP))
                   ,try (liftA2 Jnz (string "jnz" >> spaces >> refP) (spaces >> refP))
                   ]
    refP = choice [Reg <$> letter, Val <$> intP]


main :: IO ()
main = applyInput progP solveP1 solveP2


{-

number of non primes between b and c stepping 17

0   b = 57
1   c = 57
2   if a == 0 then jump #7
3   b = b * 100 + 100_000
6   c = b + 17_000

7   f = 1
8   d = 2

do
9   e = 2
do
13  if d * e == b then f = 0
14  e++
while (e /= b)
17  d++
while (d /= b)

21  if f == 0 then h++

24  if b == c then end
25  b = b + 17
26  jump #7

-}