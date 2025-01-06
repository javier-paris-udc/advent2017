module Main where

import           AoC                              (applyInput, intP)
import           Control.Monad.Trans.State.Strict (State, execState, gets, modify)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust, fromMaybe)
import           Data.List                        (uncons)
import           Text.Parsec                      (choice, letter, sepEndBy1, spaces, string, try)
import           Text.Parsec.String               (Parser)

data Ref = Reg Char | Val Int deriving (Show, Eq)
data Inst = Snd Ref
          | Set Char Ref
          | Add Char Ref
          | Mul Char Ref
          | Mod Char Ref
          | Rcv Char
          | Jgz Ref Ref
    deriving (Show, Eq)

type Prog = Map.Map Int Inst

data St = St {regs :: Map.Map Char Int
             ,send :: [Int]
             ,sends :: Int
             ,recv :: [Int]
             ,blocked :: Bool
             ,ip :: Int
             } deriving (Show, Eq)


getReg :: Char -> State St Int
getReg r = gets (fromMaybe 0 . (Map.!? r) . regs)


getRef :: Ref -> State St Int
getRef ref = case ref of
    Val x -> pure x
    Reg r -> getReg r

sndI :: Ref -> State St ()
sndI ref = do
    val <- getRef ref
    modify (\st -> st { send = val : send st, sends = sends st + 1, ip = ip st + 1 })


biOp :: (Int -> Int -> Int) -> Char -> Ref -> State St ()
biOp op reg ref = do 
    valReg <- getReg reg
    valRef <- getRef ref
    modify (\st -> st { regs = Map.insert reg (valReg `op` valRef) (regs st), ip = ip st + 1})


setI :: Char -> Ref -> State St ()
setI = biOp (\_ x -> x)


addI :: Char -> Ref -> State St ()
addI = biOp (+)


mulI :: Char -> Ref -> State St ()
mulI = biOp (*)


modI :: Char -> Ref -> State St ()
modI = biOp mod


rcvI :: Char -> State St ()
rcvI reg = do
    recvBuf <- gets recv
    case recvBuf of
        []   -> modify (\st -> st { blocked = True })
        v:vs -> modify (\st -> st { regs = Map.insert reg v (regs st), recv = vs, ip = ip st + 1 })


jgzI :: Ref -> Ref -> State St ()
jgzI ref1 ref2 = do
    valRef1 <- getRef ref1
    valRef2 <- getRef ref2
    if valRef1 > 0 then modify (\st -> st { ip = ip st + valRef2 })
    else modify (\st -> st { ip = ip st + 1 })


doInst :: Prog -> State St ()
doInst prog = do
    instIdx <- gets ip
    case prog Map.! instIdx of
        Snd ref       -> sndI ref
        Set reg ref   -> setI reg ref
        Add reg ref   -> addI reg ref
        Mul reg ref   -> mulI reg ref
        Mod reg ref   -> modI reg ref
        Rcv reg       -> rcvI reg
        Jgz ref1 ref2 -> jgzI ref1 ref2


untilM_ :: State St () -> State St Bool -> State St ()
untilM_ st check = do
    st
    end <- check
    if end then pure ()
    else untilM_ st check


isBlocked :: State St Bool
isBlocked = gets blocked


run2 :: Prog -> St -> St -> Int
run2 prog st0 st1
    | blocked st0 =
        let newSt1 = execState (untilM_ (doInst prog) isBlocked) st1 in
        if null (send newSt1) then sends st1
        else run2 prog
                  (st0 { recv = reverse (send newSt1), blocked = False })
                  (newSt1 { send = [] })
    | otherwise =
        let newSt0 = execState (untilM_ (doInst prog) isBlocked) st0 in
        if null (send newSt0) then
            if blocked st1 then sends st1
            else run2 prog newSt0 st1
        else run2 prog
                  (newSt0 { send = [] })
                  (st1 { recv = recv st1 ++ reverse (send newSt0), blocked = False })


solveP2 :: Prog -> Int
solveP2 prog = run2 prog st0 st1
  where
    st0 = St { regs = Map.singleton 'p' 0, send = [], sends = 0, recv = [], ip = 0, blocked = False }
    st1 = St { regs = Map.singleton 'p' 1, send = [], sends = 0, recv = [], ip = 0, blocked = False }


solveP1 :: Prog -> Int
solveP1 prog = fst $ fromJust $ uncons $ send $ execState (untilM_ (doInst prog) isBlocked) st0
  where
    st0 = St { regs = Map.empty, send = [], sends = 0, recv = [], ip = 0, blocked = False }


progP :: Parser Prog
progP = do
    insts <- instP `sepEndBy1` spaces
    return $ Map.fromList $ zip [0 ..] insts
  where
    instP = choice [try (Snd <$> (string "snd" >> spaces >> refP))
                   ,try (liftA2 Set (string "set" >> spaces >> letter) (spaces >> refP))
                   ,try (liftA2 Add (string "add" >> spaces >> letter) (spaces >> refP))
                   ,try (liftA2 Mul (string "mul" >> spaces >> letter) (spaces >> refP))
                   ,try (liftA2 Mod (string "mod" >> spaces >> letter) (spaces >> refP))
                   ,try (Rcv <$> (string "rcv" >> spaces >> letter))
                   ,try (liftA2 Jgz (string "jgz" >> spaces >> refP) (spaces >> refP))
                   ]
    refP = choice [Reg <$> letter, Val <$> intP]


main :: IO ()
main = applyInput progP solveP1 solveP2