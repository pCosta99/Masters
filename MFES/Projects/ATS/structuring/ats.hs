module Main where

import Data.List
import Control.Monad
import System.Directory
import Control.Monad.Extra
import Control.Arrow
import Data.List.Split hiding (split)
import Data.Maybe
import System.Environment
import System.IO
import System.Process
import Cp
import Iso
import Control.Applicative

type Package = String

format :: FilePath -> IO [FilePath]
format d = map (d ++) . flip (\\) [".",".."] <$> getDirectoryContents d

func :: FilePath -> IO [FilePath]
func d = do (a,b) <- partitionM doesFileExist =<< format d
            let (aa, bb) = (filter filterFunc a, map (++ "/") b)
            case bb of [] -> return aa
                       x -> funcAux (aa,bb)
    where
        filterFunc x = (not . isPrefixOf "."  . awful $ x) && ("java" `isSuffixOf` x)
        awful = reverse >=< takeWhile (/= '/')

funcAux :: ([FilePath], [FilePath]) -> IO [FilePath]
funcAux (a,b) = do l <- mapM func b
                   return $ a ++ concat l

foo :: IO [(String, [FilePath])]
foo = mapM (dstr . split fullPath func) =<< dirFilesFullPath
    where strip = (++ "/src/main/java/") . reverse >=< takeWhile (/= '/') . tail
          fullPath s = (++ strip s) <$> fmap (++ "/projects_maven/") getCurrentDirectory
          dirFilesFullPath = map (snoc "/") <$> ((++ "/projectsPOO_1920/") <$> getCurrentDirectory >>= format)
          -- dirFilesFullPath = getCurrentDirectory >>= pure . (++ "/mini_projects/") >>= format >>= return . map (snoc "/")
          snoc a s = s ++ a

-- saca package do ficheiro
getPkg :: FilePath -> IO Package
getPkg = readFileOP >=> return . cutMaybe . helper . splitOn "\n"
    where cutMaybe = cond isNothing nil fromJust
          readFileOP = flip openFile ReadMode >=> (\h -> do {hSetEncoding h latin1; hGetContents h})
          replace a b = map (\x -> if x == a then b else x)
          helper = find (isPrefixOf "package") >=> return . replace '.' '/' . tail . dropWhile (/= ' ') . (reverse >=< tail . dropWhile (/= ';'))

pkgSort :: [FilePath] -> IO [(Package, [FilePath])]
pkgSort =  fmap (map (split (p1 . head) (map p2)) . groupBy factorG . sortBy factorS) . mapM (rstr . split getPkg id)
    where factorS x y = compare (p1 x) (p1 y)
          factorG x y = p1 x == p1 y


f (x, l) = concatMap (\(p, lf) -> (if p /= "" then "mkdir -p " ++ x ++ p else ""):map (\fi -> "cp \"" ++ fi ++ "\" " ++ x ++ p) lf) l

allCommands = fmap (concatMap (uncurry (:) . split ((++) "mkdir -p " . p1) f)) commands

runCommands = allCommands >>= mapM callCommand

commands = foo >>= mapM (lstr . (id >< pkgSort))

main = allCommands >>= mapM_ putStrLn
