{-# LANGUAGE OverloadedStrings #-}
module Main where

import Algebra
import Algebra.Function
import Relvar.Pretty

import Data.Time (fromGregorian)
import Data.Typeable (typeOf)
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), clearScreen, setCursorPosition, setSGR)


-- | outputs a relation to the screen
main :: IO ()
main = do initStdout "Relation-Tool: Basic Examples\n"

          colorStrLn Vivid White Vivid Blue "Elem types:\n"
          putStrLn $ "*> a_lift1= " ++ show a_lift1
          putStrLn $ "*> a_lift3= " ++ show a_lift3
          putStrLn $ "*> a_lift4= " ++ show a_lift4
          putStrLn ""

          colorStrLn Vivid White Vivid Blue "Creating relations:\n"
          putStrLn $ "*> show r_fst= " ++ show r_fst
          putStrLn ""

          colorStrLn Vivid White Vivid Blue "Showing relations:\n"
          putStrLn $ "*> viewAll r_fst"
          viewAll r_fst
          putStrLn $ "\n*> viewAll r_chinese"
          viewAll r_chinese
          putStrLn "\n"

          colorStrLn Vivid White Vivid Blue "Basic relation operators:\n"
          colorStrLn Vivid White Dull Blue "3 relations:\n"
          putStrLn $ "*> viewAll r_class"
          viewAll r_class
          putStrLn $ "\n*> viewAll r_habitat"
          viewAll r_habitat
          putStrLn $ "\n*> viewAll r_zoo"
          viewAll r_zoo
          putStrLn $ ""

          colorStrLn Vivid White Dull Blue "project operator:"
          putStrLn $ "\nproject r_class on CLASS: which classes are there?"
          let r_project = project r_class ["CLASS"]
          viewAll r_project
          putStrLn $ ""

          colorStrLn Vivid White Dull Blue "restrict operator:"
          putStrLn $ "\nrestrict r_class on mammals: which animals are mammals?"
          let f_mamm = (== "mammals") :: String -> Bool
              r_restrict = restrict r_class (liftBoolFun f_mamm) ["CLASS"]
          viewAll r_restrict
          putStrLn $ ""
          
          colorStrLn Vivid White Dull Blue "join operator:"
          putStrLn $ "\njoin r_class and r_habitat"
          let r_join = r_class `join` r_habitat
          viewAll $ r_join
          putStrLn $ ""
          
          colorStrLn Vivid White Dull Blue "restrict on 2 arguments:"
          putStrLn $ "\nrestrict r_join on mammals: which mammals live in water?"
          let f_mamm_h2o x y = (x == ("mammals"::String)) && (y == ("water"::String))
              r_restrict2 = restrict r_join (liftBoolFun2 f_mamm_h2o) ["CLASS", "HABITAT"]
          viewAll r_restrict2
          putStrLn $ ""
          
          colorStrLn Vivid White Dull Blue "summarize operator:"
          putStrLn $ "\nsummarize: # of class/habitat living at the zoo?"
          let r_joinall = r_join `join` r_zoo
              r_summary = summarize r_joinall (project r_joinall ["CLASS", "HABITAT"]) [Count] ["#"] ["ZOOTOTAL"]
          viewAll $ r_summary

-------------------------
-- getting started
-------------------------

-- 2 ways for "lifting" basic types into Elem type:
-- 1) use Elem constructors:
-- B for Bool, C for Char, S for String, T for Text, I for Int, J for Integer,
-- D for Double, DD for Day, DT for UTCTime, BS for ByteString
-- R for RelVar, A for [Elem], T2 for (Elem, Elem), T3 for (Elem, Elem, Elem)
-- and Nil for Nil.
a_lift1 = [S "foo", D 3.14159, A [J 123, DD $ fromGregorian 2016 8 19], T2 (I 456, B True)]
-- 2) or use toElem function:
a_lift2 = [toElem ("foo" :: String), toElem (3.14159 :: Double)
          , toElem [toElem (123 :: Integer), toElem $ fromGregorian 2016 8 19]
          , toElem (toElem (456 :: Int), toElem True)]
a_lift3 = map toElem ["foo" :: String, "bar", "baz"] ++ map toElem [1..10::Int]
-- Relations can be lifted into Elems too:
a_lift4 = [S "foo", I 123, R r_fst]

-- To manually create a relation:
-- enter label names (String), label types (TypeRep), then values (of 'Elem' type)
r_fst = relvar ["foo", "bar"] [typeOf (undefined::String), typeOf (undefined::Double)]
    [[S "abc", D 3.145159]]

-- Usual typeReps convenience "constants":
-- tyB for Bool, tyC for Char, tyS for String, tyT for Text, tyI for Int, tyJ for Integer,
-- tyD for Double, tyDD for Day,tyDT for UTCTime, tyBS for (strict) ByteString,
-- tyR for RelVar, tyA for [Elem], tyT2 for (Elem, Elem), tyT3 for (Elem, Elem, Elem)
-- and tyZ for Nil.
-- another way to create r_fst:
r_snd = relvar ["foo", "bar"] [typeOf tyS, typeOf tyD]
    [[S "abc", D 3.145159]]

-- use Text for Unicode characters
r_chinese = relvar ["Hanzi", "Piyin"] [tyT, tyT]
    [[T "我", S "wǒ"]
    ,[T "是", S "shí"]
    ,[T "法", S "fǎ"]
    ,[T "國", S "guō"]
    ,[T "人", S "rén"]
    ]

-- relation operation basics
r_class = relvar ["ANIMAL", "CLASS"] [tyS, tyS]
     [[S "horse", S "mammals"], [S "lion", S "mammals"], [S "hippo", S "mammals"], [S "dolphin", S "mammals"]
     ,[S "kangaroo", S "mammals"], [S "platypus", Nil]
     ,[S "crocodile", S "reptiles"], [S "turtle", S "reptiles"]
     ,[S "flamingo", S "birds"], [S "pelican", S "birds"], [S "ostrich", S "birds"]
     ]

r_habitat = relvar ["ANIMAL", "HABITAT"] [tyS, tyS]
     [[S "horse", S "land"], [S "lion", S "land"]
     ,[S "hippo", S "land"],[S "hippo", S "water"],[S "dolphin", S "water"]
     ,[S "kangaroo", S "land"],[S "platypus", S "water"]
     ,[S "crocodile", S "land"],[S "crocodile", S "water"]
     ,[S "turtle", S "water"]
     ,[S "pelican", S "air"],[S "pelican", S "water"]
     ,[S "flamingo", S "air"],[S "ostrich", S "land"]
     ]

r_zoo = relvar ["ANIMAL", "#"] [tyS, tyI]
     [[S "horse", I 3], [S "lion", I 10], [S "hippo", I 5], [S "dolphin", I 2]
     ,[S "kangaroo", I 7], [S "platypus", I 0]
     ,[S "crocodile", I 2], [S "turtle", I 123]
     ,[S "flamingo", I 13], [S "pelican", I 22], [S "ostrich", I 1]
     ]

-- filtering functions :: [Elem] -> Bool
-- manual definition:
--    unary function
f_mamms = (== S "mammals") . head
--    binary function
f_mamms2 = \ (x:y:xs) -> x == S "mammals" && y > I 7
-- via liftBoolFun (see Algebra.Function)
--    unary function
f_mamms' = liftBoolFun f_ms
--    binary function2

f_ms :: String -> Bool
f_ms = (== "mammals")
f_ms2 :: String -> Int -> Bool
f_ms2 a z = (a == "mammals") && (z > 7)

myrel = relvar ["foo", "bar"] [typeOf (undefined::String), typeOf (undefined::Double)]
        [[S "abc", D 3.145159]
        ,[S "xyz", D 2.718281]
        ]
mytable = table myrel (Just ["foo","bar"]) Nothing Nothing

        
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