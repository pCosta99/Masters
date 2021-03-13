module Main where

import Z3.Monad
import Control.Monad
import Control.Arrow
import Data.Maybe
import qualified Data.Map as M
import Data.List.Split
import Data.List (intercalate)

data SMT = SMT {dim :: Dim, vars :: Vars, locked :: Blocked, goals :: Goals} deriving (Show, Eq)
type Dim = (Int,Int)
type Vars = [(Int,Int)]
type Goals = [(Either Int Int,Integer)]
type Blocked = [((Int,Int),Int)]

p2s (x,y) = show x ++ "_" ++ show y
p2l (x,y) = [x,y]
l2p [x,y] = (x,y)
l2p _ = error "The first line is wrong! Please put only number of \"LINES COLS\""

limit lb ub i = p2l (mkLe lb i, mkLe i ub)

-- Right retrieves a line, Left retrieves a column
access (l,c) (Right x) xs = drop ((x-1)*c) . take (x*c) $ xs
access (l,c) (Left x) xs = map ((xs!!) . pred) (enumFromThenTo x (x+c) (l*c))

setGoal dim v lc g = assert =<< join (liftM2 mkEq (mkInteger g) (mkAdd $ access dim lc v))

script :: SMT -> Z3 (Maybe [Integer])
script (SMT dim v locked goals) = do
    vars <- mapM mkFreshIntVar $ p2s <$> v
    let dict = M.fromList $ zip v vars
    ints <- mapM mkInteger [1 .. toInteger $ uncurry (*) dim]
    assert =<< mkDistinct vars -- All distinct
    assert =<< mkAnd =<< sequence (limit (head ints) (last ints) =<< vars) -- boundries
    assert =<< mkAnd =<< mapM (uncurry mkEq . ((dict M.!) *** ((ints!!) . pred))) locked -- initial values
    mapM (uncurry (setGoal dim vars)) goals -- goal restrictions
    fmap snd $ withModel $ \m -> catMaybes <$> mapM (evalInt m) vars -- run model

smtFromFile :: String -> IO SMT
smtFromFile path = do
    ((l,c), m1) <- ((read *** read) . l2p . head &&& tail) . map words . lines <$> readFile path
    let (m2,g) = (concat . map fst &&& (++ last m1) . map snd) . map (init &&& last) $ init m1
    let indexes = liftM2 (,) [1..l] [1..c]
    let locked = map (id *** read) . filter ((/= "*") . snd) $ zip indexes m2
    let goals = zip (map Right [1..l] ++ map Left [1..c]) (map read g)
    return $ SMT (l,c) indexes locked goals

main :: IO ()
main = do
    file <- putStr "Filename: " >> getLine
    smt <- smtFromFile file
    evalZ3 (script smt) >>= \mbSol ->
        case mbSol of
             Nothing  -> error "No solution found."
             Just sol -> putStrLn "\nSolution: " >> mapM_ (putStrLn . intercalate " " . map show) (chunksOf (snd $ dim smt) sol)
