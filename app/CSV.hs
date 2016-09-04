module Main where

import Algebra
import Relvar.Pretty
import qualified Relvar.CSV as CSV (CSVSettings(..), toRelation)
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), clearScreen, setCursorPosition, setSGR)

-- | Imports csv files into relation and additional operations on relations.
main :: IO ()
main = do initStdout "relation-tool library: CSV Examples\n"
          -- these details must be provided to properly import csv into relations
          let defPath1 = "./app/olympics2016_results.csv"
              defPath2 = "./app/olympics2016_medalists.csv"
              -- see https://janthelme.github.io/relation-tool/relation-tool-0.1.0.0/Relvar.html#g:10
              -- for the full list of TypeRep representatives (tyS, tyB, etc...).
              cs1 = ["SPORT","EVENT","MEDALIST","MEDAL"]
              tys1 = [tyS,tyS,tyS,tyS]
              cs2 = ["MEDALIST","COUNTRY","ISTEAM"]
              tys2 = [tyS,tyS,tyB]

          r1 <- CSV.toRelation defPath1 ';' cs1 tys1
          r2 <- CSV.toRelation defPath2 ';' cs2 tys2

          let rj = r1 `join` r2
              rp = project rj ["COUNTRY"]
              rs = summarize rj rp [Count] ["COUNTRY"] ["MEDALTOTAL"]
              tbl = table rs Nothing (Just [("MEDALTOTAL", Desc)]) $ Just ([0..10] ++ [(card rs) -1])

          outputrelvar_demo r1 "r1" [0..9]
          outputrelvar_demo r2 "r2" [0..9]
          outputtable_demo tbl "results" [0..]

-------------------------
-- misc. presentation
-------------------------
-- outputs a relation and additional details
outputrelvar_demo :: Relvar -> String -> [Int] -> IO()
outputrelvar_demo r nm rwIdxs = do colorStrLn Vivid White Vivid Blue $ "Relation " ++ nm ++ ":\n"
                                   putStrLn $ "Attributes: " ++ (show $ attributes r)
                                   putStrLn ("# of rows: " ++ (show $ card r) ++ "\n")
                                   view r rwIdxs
                                   putStrLn "\n"

-- outputs a table and additional details
outputtable_demo :: Table -> String -> [Int] -> IO()
outputtable_demo tbl nm rwIdxs = do
    colorStrLn Vivid White Vivid Blue $ "Table " ++ nm ++ ":\n"
    putStrLn $ "Columns: " ++ (show $ header tbl)
    putStrLn ("# of rows: " ++ (show $ length $ tdata tbl) ++ "\n")
    view tbl rwIdxs
    putStrLn "\n"

-------------------------
-- stdout functions - only for presentation purpose.
-------------------------
-- Clear screen, add title
initStdout :: String -> IO()
initStdout title = do
      clearScreen
      setCursorPosition 0 0
      colorStrLn Vivid White Vivid Red title

-- Text version
colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
  putStr str
  setSGR []
  putStrLn ""





