{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{---------------------------------------------------------------------
Salsa parser tests.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module SalsaParserTests where

import Test.QuickCheck
import Control.Monad.State
import Data.List (intercalate)

import SalsaAst
import SalsaParser (parseString)
import SalsaInterp (runProg)
import SalsaGenerator


----------------------------------------------------------------------
--- QuickCheck testing
----------------------------------------------------------------------

propSemanticallyEqual :: Property
propSemanticallyEqual =
  forAll arbitraryProgram $
  \p -> forAll (showProgram p) $
        \s ->
        case parseString s of
          Right p' -> runProg 1 p == runProg 1 p'
          Left _ -> False


----------------------------------------------------------------------
--- Other testing
----------------------------------------------------------------------

rightCheck :: (String, Program) -> IO ()
rightCheck (s, p) = case parseString s of
  Left e -> error ("parse error in:\n" ++ s ++ "\nerror:\n" ++ show e)
  Right p0 -> unless (p == p0)
              $ error ("parse error:\n" ++ intercalate "\n" (map show p)
                       ++ "\n!=\n" ++ intercalate "\n" (map show p0))

wrongCheck :: String -> IO ()
wrongCheck p | not wrongCheck' = error ("missing parse error in: " ++ p)
             | otherwise = return ()
  where wrongCheck' = case parseString p of
          Left _ -> True
          Right _ -> False

rights :: [(String, Program)]
rights = [
  -- precedence
  ("a->(0,0) || b->(0,0) @ A",
   [Com $ Par (Move ["a"] (Abs (Const 0) (Const 0)))
    (At (Move ["b"] (Abs (Const 0) (Const 0))) "A")]
  )
  ,
  -- multi
  ("viewdef A 400 -4 ((400 + 2)) \ncircle tau 1 1 (2 - 1 + 44) green \n\
\rectangle beta tau.x tau.y tau.y + 4 tau.x + 3 + 1 orange \ntau beta -> (0, 0)",
   [ Def $ Viewdef "A" (Minus (Const 400) (Const 4)) (Plus (Const 400) (Const 2))
   , Def $ Circle "tau" (Const 1) (Const 1)
     (Plus (Minus (Const 2) (Const 1)) (Const 44)) Green
   , Def $ Rectangle "beta" (Xproj "tau") (Yproj "tau")
     (Plus (Yproj "tau") (Const 4))
     (Plus (Plus (Xproj "tau") (Const 3)) (Const 1)) Orange
   , Com $ Move ["tau", "beta"] $ Abs (Const 0) (Const 0)
   ]
  )
  ]

wrongs :: [String]
wrongs = [
  -- simply wrong
  "meh"
  ,
  -- missing a space character
  "viewA"
  ,
  -- group definition without end
  "group Bouta [ Arr   \nBrr"
  ,
  -- premature bracket command end
  "{a -> (3,3  ) ||} b -> (4,4+1)"
  ,
  -- wrong Expr operator
  "circle hej 2  + 5 (1 * 88) muh.x - 14 blue"
  ,
  -- real instead of integer
  " a -> +(3.4, 0)"
  ,
  -- missing separator comma
  " baa -> (2 22)\n"
  ,
  -- wrong colour
  " rectangle\tmeat 0 0 0 0 pink"
  ,
  -- using a reserved word
  "circle circle 3 3 3 blue"
  ]



----------------------------------------------------------------------
--- Command line entry point
----------------------------------------------------------------------

runMain :: IO ()
runMain = do
  putStrLn "Running parser tests."
  forM_ rights rightCheck
  forM_ wrongs wrongCheck
  putStrLn "All manual parser tests pass."
  putStrLn "Testing: semantic equality"
  quickCheckWith (stdArgs { maxSuccess = 1000 }) propSemanticallyEqual
