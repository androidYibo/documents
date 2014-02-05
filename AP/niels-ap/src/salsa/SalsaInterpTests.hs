{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{---------------------------------------------------------------------
Salsa interpreter tests.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module SalsaInterpTests where

import Test.QuickCheck
import Control.Applicative hiding (Const)
import Data.List
import Control.Monad
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as S

import Misc
import Gpx
import SalsaAst
import SalsaInterp
import SalsaParser
import SalsaGenerator


----------------------------------------------------------------------
--- QuickCheck testing
----------------------------------------------------------------------

quickCheckTest :: (String, Property) -> IO ()
quickCheckTest (name, prop) = do
  putStrLn ("Testing " ++ name)
  r <- quickCheckWithResult (stdArgs { maxSuccess = 1000 }) prop
  case r of
    Success {} -> return ()
    _ -> error "quickCheck failed"


-- Check interpolate.
propInterpolate :: Property
propInterpolate = forAll t f
  where f (n, p0, p1) = fromIntegral (length xs) == n
                        && (sort xs == xs || sort xs == reverse xs)
                        && (n == 0 || p1 == last xs)
          where xs = interpolate n p0 p1
        t :: Gen (Integer, Position, Position)
        t = (,,) <$> choose (0, 70) <*> arbitraryPosition
            <*> arbitraryPosition


-- The number of frames must be equal to the number of non-parallel moves times
-- the framerate.
propFps :: Property
propFps = forAll t f
  where f (fps, p) = nFrames (runProg fps p)
                     == fromIntegral fps * numberOfTopCommands p
        t :: Gen (Integer, Program)
        t = (,) <$> choose (1, 68) <*> arbitraryProgram

numberOfTopCommands :: Program -> Int
numberOfTopCommands = length . filter isCom
  where isCom (Com _) = True
        isCom _ = False


-- All views of an AST must also be in the animation.
propViews :: Property
propViews = forAll arbitraryProgram $ \p ->
  sort (progViews p) == sort (animViews (runProg 1 p))

progViews :: Program -> [Ident]
progViews = mapMaybe progViews'
  where progViews' (Def (Viewdef i _ _)) = Just i
        progViews' _ = Nothing

animViews :: Animation -> [Ident]
animViews (vs, _) = map (\(i, _, _) -> i) vs


-- When a shape occurs in the AST, it must also occur in at least one frame in
-- the animation (because the program generator makes sure to move all shapes at
-- least once).
propShapeOccurence :: Property
propShapeOccurence = forAll arbitraryProgramNoProj f
  where f p = S.fromList (progShapes p) == S.fromList (animShapes (runProg 1 p))

data SShape = SRect Integer Integer String
            | SCirc Integer String
            deriving (Show, Eq, Ord)

progShapes :: Program -> [SShape]
progShapes = mapMaybe progShapes'
  where progShapes' dc = case dc of
          Def (Rectangle _ _ _ w h col) ->
            Just $ SRect (fromJust $ tryEvalExpr w)
            (fromJust $ tryEvalExpr h) $ getColourName col
          Def (Circle _ _ _ r col) ->
            Just $ SCirc (fromJust $ tryEvalExpr r) $ getColourName col
          _ -> Nothing

animShapes :: Animation -> [SShape]
animShapes = mapMaybe animShapes' . concat . snd
  where animShapes' instr = case instr of
          DrawRect _ _ w h _ col -> Just $ SRect w h col
          DrawCirc _ _ r _ col -> Just $ SCirc r col


-- When a move-to-position is absolute, there must be at least one frame in
-- which a shape is at that position.
propAbsPos :: Property
propAbsPos = forAll arbitraryProgramNoProj f
  where f p = all good $ absPoss p
          where good (e0, e1) = animHasPos (fromJust $ tryEvalExpr e0,
                                            fromJust $ tryEvalExpr e1)
                                $ runProg 1 p

absPoss :: Program -> [(Expr, Expr)]
absPoss = mapMaybe absPoss'
  where absPoss' (Com (Move _ (Abs a b))) = Just (a, b)
        absPoss' _ = Nothing

animHasPos :: Position -> Animation -> Bool
animHasPos (x, y) (_, frms) = any hasPos $ concat frms
  where hasPos instr = case instr of
          DrawRect x0 y0 _ _ _ _ -> x0 == x && y0 == y
          DrawCirc x0 y0 _ _ _ -> x0 == x && y0 == y


qcProps :: [(String, Property)]
qcProps = [ ("interpolate", propInterpolate)
          , ("fps", propFps)
          , ("views", propViews)
          , ("shape", propShapeOccurence)
          , ("abs", propAbsPos)
          ]


----------------------------------------------------------------------
--- Manual tests
----------------------------------------------------------------------

type ManualTest = (Integer, String, Animation)

progCheck :: ManualTest -> IO ()
progCheck (fps, s, a) =
  when (asort a' /= asort a) $
  error ("program does not create expected animation:\n"
         ++ "program:\n" ++ intercalate "\n" (map show p) ++ "\n"
         ++ "expected animation:\n" ++ fmtAni a ++ "\n"
         ++ "actual animation:\n" ++ fmtAni a'
        )
  where a' = runProg fps p
        p = either (\e -> error ("program did not parse:\n"
                                 ++ show e ++ "\nprogram:\n" ++ s))
            id $ parseString s
        fmtAni ani = show (fst ani) ++ "\n" ++ intercalate "\n"
                     (map show (snd ani))
        asort (x, y) = (sort x, map sort y)


-- Simple, small test to see if the very basics work.
testSimple :: ManualTest
testSimple = (
  1,
  "viewdef Mu 100 100\nrectangle x 10 10 10 10 plum\tx->(10,10)",
  ([("Mu", 100, 100)],
   [ [ DrawRect 10 10 10 10 "Mu" "plum"
     ]
   ])
  )

        
-- Animation interpolating.
testAnimateInterpolate :: ManualTest
testAnimateInterpolate = (
  4,
  "viewdef Tt 300 333\n\
\circle circa 10 90 20 blue\n\
\circa -> (10, 10)",
  ([("Tt", 300, 333)], 
   [ [ DrawCirc 10 70 20 "Tt" "blue"
     ]
   , [ DrawCirc 10 50 20 "Tt" "blue"
     ]
   , [ DrawCirc 10 30 20 "Tt" "blue"
     ]
   , [ DrawCirc 10 10 20 "Tt" "blue"
     ]
   ])
  )


-- Shapes
testShapes :: ManualTest
testShapes = (
  1,
  "viewdef Niels 1338 1336\ncircle c 1 2 3 blue\
\\nrectangle r 1 2 3 4 plum\nc r->+(0,0)",
  ([("Niels", 1338, 1336)],
   [ [ DrawCirc 1 2 3 "Niels" "blue"
     , DrawRect 1 2 3 4 "Niels" "plum"
     ]
   ])
  )


-- Multiple moves.
testMultipleMoves :: ManualTest
testMultipleMoves = (
  1,
  "viewdef A 1 1 circle x 1 1 1 orange x -> (2, 2) x -> +(1, 1)",
  ([("A", 1, 1)],
   [ [ DrawCirc 2 2 1 "A" "orange"
     ]
   , [ DrawCirc 3 3 1 "A" "orange"
     ]
   ])
  )


-- Groups.
testGroups :: ManualTest
testGroups = (
  1,
  "viewdef A 3 3\n\
\viewdef B 5 5\n\
\viewdef Otto 999 991\n\
\group Brandenburg [Otto A B]\n\
\view Brandenburg\n\
\circle x 0 0 25 green\n\
\view B\n\
\x -> (1, 10)\n\
\view Brandenburg\n\
\x->+(5,5)",
  ([("Otto", 999, 991), ("B", 5, 5), ("A", 3, 3)],
   [ [ DrawCirc 0 0 25 "Otto" "green"
     , DrawCirc 0 0 25 "A" "green"
     , DrawCirc 1 10 25 "B" "green"
     ]
   , [ DrawCirc 5 5 25 "Otto" "green"
     , DrawCirc 5 5 25 "A" "green"
     , DrawCirc 6 15 25 "B" "green"
     ]
   ])
  )


-- Use of @.
testAt :: ManualTest
testAt = (
  1,
  "viewdef I 10 10 circle a 1 1 1 red viewdef Y 2 2\n a->(0,0) @ I",
  ([("I", 10, 10), ("Y", 2, 2)],
   [ [ DrawCirc 0 0 1 "I" "red"
     ]
   ])
  )


-- Use of ||.
testPar :: ManualTest
testPar = (
  1,
  "viewdef Uha 100 100\t circle a 1 1 1 red\t rectangle b 1 1 1 1 red\
\\t a -> (7, 7) || b -> +(8, 1)",
  ([("Uha", 100, 100)],
   [ [ DrawCirc 7 7 1 "Uha" "red"
     , DrawRect 9 2 1 1 "Uha" "red"
     ]
   ])
  )


-- Expr.
testExpr :: ManualTest
testExpr = (
  1,
  "viewdef Thing 3 + 9-11 + 500 ( (300))\n\
\circle b 2 3 4 green\n\
\rectangle rrr 4 4 b.x + 14 (4 + b.y - b.y + b.y) red\n\
\b rrr -> +(1,0-0)",
  ([("Thing", 501, 300)],
   [ [ DrawCirc 3 3 4 "Thing" "green"
     , DrawRect 5 4 16 7 "Thing" "red"
     ]
   ])
  )



progs :: [ManualTest]
progs = [ testSimple
        , testAnimateInterpolate
        , testShapes
        , testMultipleMoves
        , testGroups
        , testAt
        , testPar
        , testExpr
        ]


----------------------------------------------------------------------
--- Command line entry point
----------------------------------------------------------------------

runMain :: IO ()
runMain = do
  putStrLn "Running interpreter tests."
  forM_ progs progCheck
  putStrLn "All manual interpreter tests pass."
  forM_ qcProps quickCheckTest
