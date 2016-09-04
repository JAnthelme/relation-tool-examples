-- {-# LANGUAGE OverloadedStrings #-}
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

import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), clearScreen, setCursorPosition, setSGR)

-- | outputs a relation to the screen
main :: IO ()
main = do initStdout "relation-tool C.J. Date examples from \"An Introduction to Database Systems\" Eighth Edition\n"
          colorStrLn Vivid White Vivid Blue "union / intersection / minus operators - Fig 7.2"
          colorStrLn Vivid Black Vivid Green "\nrelation A"
          viewAll rA
          colorStrLn Vivid Black Vivid Green "\nrelation B"
          viewAll rB
          colorStrLn Vivid Black Vivid White "\nA union B"
          viewAll $ rA `union` rB
          colorStrLn Vivid Black Vivid White "\nA intersection B"
          viewAll $ rA `intersection` rB
          colorStrLn Vivid Black Vivid White "\nA minus B"
          viewAll $ rA `minus` rB
          colorStrLn Vivid Black Vivid White "\nB minus A"
          viewAll $ rB `minus` rA
          putStrLn ""

          colorStrLn Vivid White Vivid Blue "times operator - Fig 7.3"
          colorStrLn Vivid Black Vivid Green "\nrelation A"
          viewAll r_prodA
          colorStrLn Vivid Black Vivid Green "\nrelation B"
          viewAll r_prodA
          colorStrLn Vivid Black Vivid White "\nA times B (part of)"
          view (r_prodA `times` r_prodB) [0..9]
          putStrLn ""

          colorStrLn Vivid White Vivid Blue "restrict operator - Fig 7.4"
          putStrLn ""
          colorStrLn Vivid Black Vivid Green "relation S"
          viewAll r_S
          putStrLn ""
          colorStrLn Vivid Black Vivid White "restrict S on CITY=\"London\""
          let fS = (== "London")
          viewAll (restrict r_S (liftBoolFun fS) ["CITY"])
          putStrLn ""
          colorStrLn Vivid Black Vivid Green "relation P"
          viewAll r_P
          putStrLn ""
          colorStrLn Vivid Black Vivid White "restrict P on Weight<14.0"
          let fP = (<= 14.0) :: Double -> Bool
          viewAll (restrict r_P (liftBoolFun fP) ["WEIGHT"])
          putStrLn ""
          colorStrLn Vivid Black Vivid Green "relation SP"
          viewAll r_SP
          putStrLn ""
          colorStrLn Vivid Black Vivid White "restrict SP on S#==\"S6\" OR P#==\"P7\""
          let fSP = (\x y-> x=="S6" || y=="P7") :: String -> String -> Bool
          viewAll (restrict r_SP (liftBoolFun2 fSP) ["S#","P#"])
          putStrLn ""

          colorStrLn Vivid White Vivid Blue "projection operator - Fig 7.5"
          putStrLn ""
          colorStrLn Vivid Black Vivid White "project S over \"CITY\""
          viewAll $ project r_S ["CITY"]
          putStrLn ""
          colorStrLn Vivid Black Vivid White "project P over \"COLOR\" and \"CITY\""
          viewAll $ project r_P ["CITY","COLOR"]
          putStrLn ""
          colorStrLn Vivid Black Vivid White "restrict S on CITY=\"Paris\" and project on \"S#\""
          let fS = (== "Paris")
          viewAll (restrict r_S (liftBoolFun fS) ["CITY"] `project` ["S#"])
          putStrLn ""

          colorStrLn Vivid White Vivid Blue "join operator - Fig 7.6"
          putStrLn ""
          colorStrLn Vivid Black Vivid White "join S and P"
          viewAll $ r_S `join` r_P
          putStrLn ""

          -- YES! The divide operator is missing...
          -- TODO: add semijoin and semiminus

          colorStrLn Vivid White Vivid Blue "extend operator - Fig 7.9"
          putStrLn ""
          colorStrLn Vivid Black Vivid White "extend P with `GMWT=WEIGHTx454`"
          let fext = (*454) :: Double -> Double
          viewAll $ extend r_P (liftEl fext) ["WEIGHT"] "GMWT"
          putStrLn ""

          colorStrLn Vivid White Vivid Blue "summarize operator - Fig 7.11"
          putStrLn ""
          colorStrLn Vivid Black Vivid White "summarize SP per \"P#\" and sum over \"QUANTITY\" into \"TOTQTY\""
          viewAll $ summarize r_SP (project r_SP ["P#"]) [Sum] ["QTY"] ["TOTQTY"]
          putStrLn ""

          colorStrLn Vivid White Vivid Blue "group operator - Fig 7.12"
          putStrLn ""
          colorStrLn Vivid Black Vivid White "group SP by \"S#\""
          let r_group = group r_SP ["P#", "QTY"] "PQ"
          viewAll r_group
          putStrLn ""

          colorStrLn Vivid White Vivid Blue "ungroup operator"
          putStrLn ""
          colorStrLn Vivid Black Vivid White "ungrouping of previous group operation"
          let r_ungroup = ungroup r_group "PQ"
          viewAll r_ungroup
          putStrLn ""
          putStrLn $ "test: is SP equal to (ungroup $ group SP): " ++ (show $ r_SP == r_ungroup)


-------------------------
-- relations
-------------------------

-- union, etc
rA = relvar ["S#", "SNAME", "STATUS", "CITY"] [tyS,tyS,tyI,tyS]
            [[S "S1", S "Smith", I 20, S "London"]
            ,[S "S4", S "Clark", I 20, S "London"]
            ]
rB = relvar ["S#", "SNAME", "STATUS", "CITY"] [tyS,tyS,tyI,tyS]
            [[S "S1", S "Smith", I 20, S "London"]
            ,[S "S2", S "Jones", I 10, S "Paris"]
            ]

-- times
r_prodA = relvar ["S#"] [tyS] [[S "S1"],[S "S2"],[S "S3"],[S "S4"],[S "S5"],[S "S6"]]
r_prodB = relvar ["P#"] [tyS] [[S "P1"],[S "P2"],[S "P3"],[S "P4"],[S "P5"],[S "P6"]]

-- back-cover
r_S = relvar ["S#", "SNAME", "STATUS", "CITY"] [tyS,tyS,tyI,tyS]
            [[S "S1", S "Smith", I 20, S "London"]
            ,[S "S2", S "Jones", I 10, S "Paris"]
            ,[S "S3", S "Blake", I 30, S "Paris"]
            ,[S "S4", S "Clark", I 20, S "London"]
            ,[S "S5", S "Adams", I 30, S "Athens"]
            ]

r_P = relvar ["P#", "PNAME", "COLOR", "WEIGHT", "CITY"] [tyS,tyS,tyS,tyD,tyS]
            [[S "P1", S "Nut",   S "Red",   D 12.0, S "London"]
            ,[S "P2", S "Bolt",  S "Green", D 17.0, S "Paris"]
            ,[S "P3", S "Screw", S "Blue",  D 17.0, S "Oslo"]
            ,[S "P4", S "Screw", S "Red",   D 14.0, S "London"]
            ,[S "P5", S "Cam",   S "Blue",  D 12.0, S "Paris"]
            ,[S "P6", S "Cog",   S "Red",   D 19.0, S "London"]
            ]

r_SP = relvar ["S#", "P#", "QTY"] [tyS,tyS,tyI]
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








