module SudokuSolver where

import Data.List
import Control.Monad
import Control.Arrow

type Var = (Int,Int,Int,Int) -- (l,c,n,sign)
data Setup = S { ls :: [Int], cols :: [Int], range :: [Int], mat :: [Var] } deriving (Show)
newtype SatSol = SS { unSS :: [[Int]] }

instance Show SatSol where
    show (SS m) = unlines $ map ((++ "0"). concatMap ((++ " ") . show)) m

-- Ensure that there are no repeated per line, column or submatrix and exactly 1 number per square
allFilled, noRepLine, noRepCol, noRepSquare, noRepSub :: Setup -> [[Var]]
allFilled (S l c r _) = [ [ (l',c',n',1) | n' <- r ] | l' <- l, c' <- c ]
noRepLine (S l c n _) = [ [ (l',c',n',1) | c' <- c ] | l' <- l, n' <- n ]
noRepCol  (S l c n _) = [ [ (l',c',n',1) | l' <- l ] | c' <- c, n' <- n ]

noRepSquare (S l c n _) = concat [ concat [ perNumber (l',c',n') | n' <- n ] | l' <- l, c' <- c] where
    perNumber t@(l',c',n') = [ [(l',c',n',-1), (l',c',n'',-1)] | n'' <- n \\ [n'] ]

noRepSub (S l c n _) = concat [  [ map (\(a,b) -> (a,b,n',1) ) r' | n' <- n ] | r' <- r ] where
    r = map createPairs . (uncurry zip . (id &&& tail)) $ enumFromThenTo 0 (isqrt $ length l) (length l)
    isqrt = floor . sqrt . fromIntegral
    createPairs (lb,ub) = [(a,b) | a <- [lb..ub-1], b <- [lb..ub-1]]

identifyVar :: (Int,Int,Int) -> Var -> Int
identifyVar (ln, cn, nn) (l,c,n,s) = s * ((l * (cn*nn)) + (c * cn) + n)

reConvert :: Int -> Int -> (Int,Int,Int)
reConvert n i = (line,col,number) where
    (line,lm) = ((i-1) `div` (n*n), (i-1) `mod` (n*n))
    (col, cm) = (max (lm `div` n) 0, lm `mod` n)
    number = cm + 1

readSol :: String -> IO ()
readSol = readFile >=> mapM_ (putStrLn . show) . map (reConvert 4) . filter (>0) . init . map (\x -> read x :: Int) . words . (!!1) . lines

main n file = do
    baseSetup <- map (\(a,b,c) -> (a,b,c,1)) . map read . lines <$> readFile file
    let setup = S [0..n-1] [0..n-1] [1..n] baseSetup
    let conds = concatMap ($ setup) [allFilled, noRepSub, noRepSquare, noRepCol, noRepLine, map (:[]) . mat]
    let satReady = SS $ nubBy (\l1 l2 -> l1 == l2 || l1 == reverse l2) $ map (map (identifyVar (n,n,n))) conds
    let (nvars, nclauses) = (n*n*n, length $ unSS satReady)
    writeFile "sudoku.cnf" ("p cnf " ++ show nvars ++ " " ++ show nclauses ++ "\n" ++ show satReady)
