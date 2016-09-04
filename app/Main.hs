{-# LANGUAGE OverloadedStrings #-}
module Main where

import Algebra
import Algebra.Function
import Relvar.Pretty
import qualified Relvar.CSV as CSV (CSVSettings(..), toRelation)

import Data.Time (fromGregorian) -- (Day, UTCTime)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Word (Word8)
import qualified Data.ByteString as B (pack)
import qualified Data.Set as Set (size)

-- import Data.ByteString.UTF8 (fromString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.IO as T (putStrLn, putStr, writeFile)

import System.Console.ANSI
-- TBD: import System.Directory (getCurrentDirectory)

-- CSV
{-
import Data.CSV.Conduit
import qualified Data.Vector as V
import qualified Data.ByteString as BS
-}

-- | outputs a relation to the screen
main2 :: IO ()
main2 = do let rel = rs
          -- T.writeFile "/home/user8/temp/relview.txt" $ render rel
           clearScreen
           setCursorPosition 0 0
           colorStrLn Vivid White Vivid Red $ "Relation Examples...\n" 
           viewAll rel
           putStrLn "\n"

-- Text version
colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
  putStr str
  setSGR []
  putStrLn ""


-- generate Elems or [Elem]
ex01 = toElem (3.14159 :: Double)
ex02 = map toElem ['A', 'B', 'C']
ex03 = map toElem ["AA"::String,"BB"]
ex04 = toElem (fromGregorian 2016 8 19)
ex05 = toElem dt
    where dt = UTCTime {utctDay = fromGregorian 2016 8 19, utctDayTime = secondsToDiffTime ((8*60 + 15)*60)}
ex06 = toElem $ B.pack [72, 69, 76, 76, 79, 32, 87, 79, 82, 76, 68]
ex07 = toElem (S "Tokyo", I 9262046)
ex08 = toElem (S "Tokyo", I 9262046, B True)
ex09 = [ex07, ex08]

ex10 = [I 1, C 'A', D 3.14159, ex05] :: [Elem]
ex11 = toElem ex10 :: Elem

ex12 = [T ("a"::Text)] :: [Elem]
xxx = "我" :: Text
r_chinese = relvar ["Hanzi", "Piyin"] [tyS, tyT] [[T "我", S "wo"]
                                                 ,[T "是", S "shi"]
                                                 ,[T "法", S "fa"]
                                                 ,[T "國", S "guo"]
                                                 ,[T "人", S "ren"]
                                                 ]

-- JC Date examples:
rA = relvar ["S#", "SNAME", "STATUS", "CITY"] [tyS,tyS,tyI,tyS] [[S "S1", S "Smith", I 20, S "London"],
                                                                 [S "S4", S "Clark", I 20, S "London"]
                                                                ]
rB = relvar ["S#", "SNAME", "STATUS", "CITY"] [tyS,tyS,tyI,tyS] [[S "S1", S "Smith", I 20, S "London"],
                                                                 [S "S2", S "Jones", I 10, S "Paris"]
                                                                ]

rel_S = relvar ["S#", "SNAME", "STATUS", "CITY"] [tyS,tyS,tyI,tyS] 
            [[S "S1", S "Smith", I 20, S "London"]
            ,[S "S2", S "Jones", I 10, S "Paris"]
            ,[S "S3", S "Blake", I 30, S "Paris"]
            ,[S "S4", S "Clark", I 20, S "London"]
            ,[S "S5", S "Adams", I 30, S "Athens"]
            ]

rel_P = relvar ["P#", "PNAME", "COLOR", "WEIGHT", "CITY"] [tyS,tyS,tyS,tyD,tyS] 
            [[S "P1", S "Nut",   S "Red",   D 12.0, S "London"]
            ,[S "P2", S "Bolt",  S "Green", D 17.0, S "Paris"]
            ,[S "P3", S "Screw", S "Blue",  D 17.0, S "Oslo"]
            ,[S "P4", S "Screw", S "Red",   D 14.0, S "London"]
            ,[S "P5", S "Cam",   S "Blue",  D 12.0, S "Paris"]
            ,[S "P6", S "Cog",   S "Red",   D 19.0, S "London"]
            ]
                                                                   
rel_SP = relvar ["S#", "P#", "QTY"] [tyS,tyS,tyI] 
            [[S "S1", S "P1", I 300]
            ,[S "S1", S "P2", I 200]
            ,[S "S1", S "P3", I 400]
            ,[S "S1", S "P4", I 200]
            ,[S "S1", S "P5", I 100]
            ,[S "S1", S "P6", I 100]
            ,[S "S2", S "P1", I 300]
            ,[S "S2", S "P2", I 400]
            ,[S "S3", S "P2", I 200]
            ,[S "S4", S "P2", I 200]
            ,[S "S4", S "P4", I 300]
            ,[S "S4", S "P5", I 400]
            ]





-- Fig 7.2 p181                                                                
r_union = union rA rB
r_union_rowno = card r_union
r_union_colno = degree r_union

r_intersect = intersection rA rB
r_intersect_rowno = card r_intersect
r_intersect_colno = degree r_intersect

r_AminusB = minus rA rB
r_AminusB_rowno = Set.size $ rdata r_AminusB

r_BminusA = minus rB rA
r_BminusA_rowno = Set.size $ rdata r_BminusA

-- Fig 7.3 p183 - Cartesian product examples
rel_prodA = relvar ["S#"] [tyS] [[S "S1"],[S "S2"],[S "S3"],[S "S4"],[S "S5"],[S "S6"]]
rel_prodB = relvar ["P#"] [tyS] [[S "P1"],[S "P2"],[S "P3"],[S "P4"],[S "P5"],[S "P6"]]
r_prod = rel_prodA `times` rel_prodB

-- Fig 7.4 p184 - Restriction examples
r_restrict = restrict rA f_restrict1 ["STATUS"]
f_restrict0 :: Elem -> Bool
f_restrict0 (I x) = x <= 20
f_restrict0 _ = False
f_restrict1 :: [Elem] -> Bool
f_restrict1 (x:[]) = f_restrict0 x 
f_restrict1 (x:xs) = False

-- Fig 7.5 p185 - Projection examples
r_projectS = project rel_S ["CITY"]
r_projectP = project rel_P ["COLOR", "CITY"]
r_allbutW  = projectaway rel_P ["WEIGHT"]

-- Fig 7.6 p186 - Natural join example
r_joinSP   = join rel_S rel_P


-- p196 - Semijoin examples
r_semijoinSvSP  = semiJoin rel_S $ restrict rel_SP f_restrict_sj1 ["P#"]
r_semiminusSvSP = semiMinus rel_S $ restrict rel_SP f_restrict_sj1 ["P#"]
f_restrict_sj0 (S x) = x == "P2"
f_restrict_sj0 _ = False
f_restrict_sj1 :: [Elem] -> Bool
f_restrict_sj1 (x:[]) = f_restrict_sj0 x 
f_restrict_sj1 (x:xs) = False

-------------------------
-- extend function examples
-------------------------

-- manual construction
f_ext_man1 :: [Elem] -> Elem
f_ext_man1 ((I x):[]) = I (10*x)
f_ext_man1 _ = Nil

-- via Function module
f_ext1' :: Int -> Int
f_ext1' = (10*)

f_ext1 :: [Elem] -> Elem
f_ext1 = liftEl f_ext1'


f_ext0 :: Elem -> Elem
f_ext0 (I x) = I (10 * x)
f_ext0 _ = Nil
f_ext12 :: [Elem] -> Elem
f_ext12 (x:[]) = f_ext0 x 
f_ext12 (x:xs) = Nil -- only works on 1 element

r_ext0 = extend rel_SP f_ext1 ["QTY"] "QTYx10"

-- Fig 7.11 p200 - Summarize example
r_summ0 = summarize rel_SP (rel_SP `project` ["P#"]) [Sum] ["QTY"] ["TOTQTY"]

-- Fig 7.12 p204 - Group example
r_group0 = group rel_SP ["P#","QTY"] "PQ"

-- Ungroup example
r_ungroup0 = ungroup r_group0 "PQ"

-- rename rA
rename_rA = rename rA ["SNAME","CITY"] ["NAME","TOWN"] 
{-
-- compute
r_compute0 = compute [Sum] ["QTY"]  ["TOTALQTY"] rel_SP
r_compute1 = compute [Sum] [""] ["TOTALQTY"] rel_SP                        -- False (issue with labels)
r_compute2 = compute [Sum] ["Q"] ["TOTALQTY"] rel_SP                       -- False (issue with labels)
r_compute3 = compute [Sum] ["QTY"] [""] rel_SP                             -- False (issue with labels)
r_compute4 = compute [Sum, Count] ["QTY","QTY"] ["TOTALQTY","COUNT"] rel_SP -- False (issue with labels)
r_compute5 = compute [Sum] ["QTY", "P#"] ["TOTALQTY","TOTALQTY"] rel_SP    -- False (issue with labels)
r_compute6 = compute [Sum] ["QTY", "P#"] ["TOTALQTY","COUNT"] rel_SP       -- False (issue with labels)
r_compute7 = compute [Sum,Count] ["QTY"] ["TOTALQTY","COUNT"] rel_SP       -- False (issue with labels)
r_compute8 = compute [Sum,Count] ["QTY", "P#"] ["TOTALQTY"] rel_SP         -- False (issue with labels)
r_compute9 = compute [Sum,Count] ["QTY", "P#"] ["TOTALQTY","COUNT"]  rel_SP
-}
-- TEST joins and stuff when there is no tuple








-------------------------
-- compute
-------------------------

r_00 = rel_SP 
{-
r_compute00 = compute [Count] ["QTY"] ["QTY_COUNT"] r_00
r_compute01 = compute [Sum] ["QTY"] ["TOTALQTY"] r_00
r_compute02 = compute [Min] ["QTY"] ["TOTALQTY"] r_00
r_compute03 = compute [Max] ["QTY"] ["TOTALQTY"] r_00
r_compute04 = compute [Max] ["P#"] ["MAXP#"] r_00
r_compute05 = compute [Avg,Min] ["QTY","P#"] ["TOTALQTY","MAXP#"] r_00
r_compute06 = compute [Avg,Min,Max,Sum] ["QTY","QTY","QTY","QTY"] ["AVGQTY","MINQ","MAXQ","SUMQ"] r_00
-}




-------------------------
-- summarize
-------------------------

rx1 = relvar ["Athlete","Medal","Event"] [tyS, tyS, tyS] 
              [[S "Bob", S "Gold", S "200 Men"]
              ,[S "Alice", S "Silver", S "200 WoMen"]
              ,[S "Bob", S "Bronze", S "100 Men"]
              ,[S "Bob", S "Bronze", S "200 Men"]
              ,[S "Alice", S "Bronze", S "Marathon Women"]
              ,[S "Eve", S "Silver", S "Marathon Women"]
              ,[S "Mallory", S "Gold", S "Marathon Men"]
              ,[S "Trent", S "Gold", S "100 Men"]
              ]
rx2 = relvar ["Country","Athlete","Age"] [tyS, tyS, tyI] 
              [[S "UK",S "Bob", I 20]
              ,[S "US",S "Alice", I 20] 
              ,[S "US",S "Eve", I 20] 
              ,[S "UK",S "Mallory", I 20] 
              ,[S "HK",S "Trent", I 20] 
              ]

rj = rx1 `join` rx2
rp = project rj ["Country"]
rs = summarize rj rp [Count] ["Medal"] ["Total"]





-------------------------
-- CSV
-------------------------



-- file path is relative to the project root directory
defPath2 = "./app/olympics2016_medalists.csv"
defPath1 = "./app/olympics2016_results.csv"

csvset :: Char ->  CSV.CSVSettings 
csvset c =  CSV.CSVSettings {CSV.csvSep  = c, CSV.csvQuoteChar = Just '"'}

cs1 = ["SPORT","EVENT","MEDALIST","MEDAL"]
tys1 = [tyS,tyS,tyS,tyS]


cs2 = ["MEDALIST","COUNTRY","ISTEAM"]
tys2 = [tyS,tyS,tyB]



outputrelvar_demo :: Relvar -> String -> [Int] -> IO()
outputrelvar_demo r nm rwIdxs = do colorStrLn Vivid White Vivid Blue $ "Relation " ++ nm ++ ":\n"
                                   putStrLn $ "Attributes: " ++ (show $ attributes r) 
                                   putStrLn ("# of rows: " ++ (show $ card r) ++ "\n")
                                   view r rwIdxs
                                   putStrLn "\n"
                           
-- outputtable_demo :: ([String], [[Elem]]) -> String -> [Int] -> IO()
outputtable_demo :: Table -> String -> [Int] -> IO()
-- outputtable_demo (lbls,rws) nm rwIdxs = do 
outputtable_demo tbl nm rwIdxs = do 
    colorStrLn Vivid White Vivid Blue $ "Table " ++ nm ++ ":\n"
    putStrLn $ "Columns: " ++ (show $ header tbl) 
    putStrLn ("# of rows: " ++ (show $ length $ tdata tbl) ++ "\n")
    view tbl rwIdxs
    putStrLn "\n"

-- | outputs a relation to the screen
main :: IO ()
main = do r1 <- CSV.toRelation defPath1 ';' cs1 tys1
          r2 <- CSV.toRelation defPath2 ';' cs2 tys2

          clearScreen
          setCursorPosition 0 0
          colorStrLn Vivid White Vivid Red $ "Relation Examples...\n" 
                             
          let rj = r1 `join` r2
              rp = project rj ["COUNTRY"]
              rs = summarize rj rp [Count] ["COUNTRY"] ["MEDALTOTAL"]

          outputrelvar_demo r1 "r1" [0..9]
          outputrelvar_demo r2 "r2" [0..9]   
           
          let tbl = table rs Nothing (Just [("MEDALTOTAL", Desc)]) $ Just ([0..10] ++ [(card rs) -1])
          outputtable_demo tbl "results" [0..]
          
          print $ keepRight r1 ["MEDAL","EVENT"]
          
          return ()
          







