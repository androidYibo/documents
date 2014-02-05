{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{---------------------------------------------------------------------
Correct Salsa Program generator.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module SalsaGenerator where

import Control.Applicative hiding (Const)
import Test.QuickCheck
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (delete, nub)

import Misc
import SalsaAst


----------------------------------------------------------------------
--- General functions for generating programs
----------------------------------------------------------------------

data MiniEnv = MiniEnv { depth :: Int
                       , views :: [(Ident, [Ident])]
                       , shapes :: M.Map Ident [Ident]
                       , notMovedShapes :: [(Ident, Ident)]
                       , activeView :: Ident
                       , noProj :: Bool -- disable Xproj and Yproj generation
                       }

emptyMiniEnv :: MiniEnv
emptyMiniEnv = MiniEnv { depth = 0
                       , views = []
                       , shapes = M.empty
                       , notMovedShapes = []
                       , activeView = error "not defined"
                       , noProj = False
                       }

type GenBuilder = StateT MiniEnv Gen

incDepth :: GenBuilder ()
incDepth = modify $ \env -> env { depth = depth env + 1 }

oneof' :: [GenBuilder a] -> GenBuilder a
oneof' = join . lift . elements

pickSomeBetween :: Eq a => Int -> Int -> [a] -> Gen [a]
pickSomeBetween a b xs = do
  n <- choose (a, b)
  nub <$> replicateM n (elements xs)

addView :: Ident -> [Ident] -> GenBuilder ()
addView gi vis = modify $ \env -> env { views = (gi, vis) : views env }

setActiveView :: Ident -> GenBuilder ()
setActiveView vi = modify $ \env -> env { activeView = vi }

getActiveViews :: GenBuilder [Ident]
getActiveViews = do
  env <- get
  return $ fromMaybe (error "meh") $ lookup (activeView env) (views env)

getAllViews :: GenBuilder [Ident]
getAllViews = concatMap snd . views <$> get

getAllActiveShapes :: GenBuilder [Ident]
getAllActiveShapes = do
  vis <- getActiveViews
  sis <- shapes <$> get
  return $ concatMap (\vi -> fromMaybe [] $ M.lookup vi sis) vis

addShape :: Ident -> GenBuilder ()
addShape si = do
  vis <- getActiveViews
  forM_ vis $ \vi ->
    modify $ \env -> env { shapes = M.update upd vi $ prep vi $ shapes env
                         , notMovedShapes = (vi, si) : notMovedShapes env
                         }
    where upd ss = Just (si : ss)
          prep vi ss | M.member vi ss = ss
                     | otherwise = M.insert vi [] ss

fork :: GenBuilder a -> GenBuilder a
fork builder = do
  old <- get
  res <- builder
  put old
  return res

workDepth :: GenBuilder a -> GenBuilder a -> GenBuilder a
workDepth fallback builder = do
  d <- depth <$> get
  if d >= depthMax
    then fallback
    else incDepth >> builder


----------------------------------------------------------------------
--- Correct arbitrary program generation 
----------------------------------------------------------------------

arbitraryProgram :: Gen Program
arbitraryProgram = evalStateT arbitraryProgramB emptyMiniEnv

arbitraryProgramNoProj :: Gen Program
arbitraryProgramNoProj = evalStateT arbitraryProgramB
                         $ emptyMiniEnv { noProj = True }
             
depthMax :: Int
depthMax = 5

arbitraryProgramB :: GenBuilder Program
arbitraryProgramB = do
  nDefComs <- lift $ choose (1, 22)
  replicateM nDefComs arbitraryDefComB <++> addEventualMoves

addEventualMoves :: GenBuilder Program
addEventualMoves = do
  ss <- notMovedShapes <$> get
  xs <- forM ss $ \(vi, si) -> do
    p <- arbitraryPosB
    return [ Def $ View vi
           , Com $ Move [si] p
           ]
  return $ concat xs

arbitraryDefComB :: GenBuilder DefCom
arbitraryDefComB = do
  vs <- views <$> get
  ss <- getAllActiveShapes
  if null vs
    then Def <$> arbitraryDefinitionB
    else oneof' [ Def <$> arbitraryDefinitionB
                , if null ss
                  then Def <$> arbitraryDefinitionB
                  else Com <$> arbitraryCommandB
                ]

arbitraryDefinitionB :: GenBuilder Definition
arbitraryDefinitionB = do
  vs <- views <$> get
  if null vs
    then viewdef
    else do
    ss <- getAllActiveShapes
    oneof' $ if null ss
             then [rectangle, circle]
             else [viewdef, rectangle, circle, view, group]
  where viewdef = do
          i <- arbitraryVIdentB
          addView i [i]
          setActiveView i
          Viewdef i <$> arbitraryExprB <*> arbitraryExprB

        rectangle = do
          i <- arbitrarySIdentB
          r <- Rectangle i <$> arbitraryExprB <*> arbitraryExprB
               <*> arbitraryExprB <*> arbitraryExprB
               <*> arbitraryColourB
          addShape i
          return r
  
        circle = do
          i <- arbitrarySIdentB
          c <- Circle i <$> arbitraryExprB <*> arbitraryExprB
               <*> arbitraryExprB <*> arbitraryColourB
          addShape i
          return c
  
        view = do
          vis <- map fst . views <$> get
          vi <- lift $ elements vis
          setActiveView vi
          return $ View vi
        
        group = do
          gi <- arbitraryVIdentB
          vis <- map fst . views <$> get
          vis' <- lift $ pickSomeBetween 1 4 vis
          return $ Group gi vis'

arbitraryCommandB :: GenBuilder Command
arbitraryCommandB = oneof' [move, at, par]
  where move = do
          sis <- getAllActiveShapes
          sis' <- lift $ pickSomeBetween 1 4 sis
          av <- getActiveViews
          forM_ av $ \vi -> forM_ sis' $ \si ->
            modify $ \env -> env { notMovedShapes = delete (vi, si)
                                                    $ notMovedShapes env }
          Move sis' <$> arbitraryPosB

        at = do
          vis <- getAllViews
          vi <- lift $ elements vis
          workDepth (At <$> move <*> return vi)
            (At <$> fork arbitraryCommandB <*> return vi)

        par = workDepth (Par <$> move <*> move)
              (Par <$> fork arbitraryCommandB <*> fork arbitraryCommandB)

arbitraryPosB :: GenBuilder Pos
arbitraryPosB = oneof' [absPos, relPos]
  where absPos = Abs <$> fork arbitraryExprB <*> fork arbitraryExprB
        relPos = Rel <$> fork arbitraryExprB <*> fork arbitraryExprB

arbitraryVIdentB :: GenBuilder Ident
arbitraryVIdentB = do
  vis <- map fst . views <$> get
  len <- lift $ choose (1, 17)
  lift $ suchThat (elements ['A'..'Z'] <:> replicateM len arbitraryChar)
    (`notElem` vis)

arbitrarySIdentB :: GenBuilder Ident
arbitrarySIdentB = do
  vis <- map fst . views <$> get
  len <- lift $ choose (1, 17)
  lift $ suchThat (elements ['a'..'z'] <:> replicateM len arbitraryChar)
    (`notElem` (vis ++ reservedWords))

arbitraryValidSIdentB :: GenBuilder Ident
arbitraryValidSIdentB = lift . elements =<< getAllActiveShapes

arbitraryChar :: Gen Char
arbitraryChar = elements ("_" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

arbitraryColour :: Gen Colour
arbitraryColour = elements $ map snd colours

arbitraryColourB :: GenBuilder Colour
arbitraryColourB = lift arbitraryColour

arbitraryExprB :: GenBuilder Expr
arbitraryExprB = do
  no <- noProj <$> get
  oneof' $ if no
           then [plus, minus, constb]
           else [plus, minus, constb, xproj, yproj]
  where plus = workDepth (Plus <$> stopper <*> stopper)
               (Plus <$> fork arbitraryExprB <*> fork arbitraryExprB)
        minus = workDepth (Minus <$> stopper <*> stopper)
               (Minus <$> fork arbitraryExprB <*> fork arbitraryExprB)
        constb = Const <$> arbitraryNumberB
        xproj = proj Xproj
        yproj = proj Yproj
        proj c = do
          ss <- getAllActiveShapes
          if null ss
            then constb
            else c <$> lift (elements ss)
        stopper = do
          no <- noProj <$> get
          if no
            then constb
            else oneof' [constb, xproj, yproj]


arbitraryNumber :: Gen Integer
arbitraryNumber = choose (0, 10^(7 :: Integer) :: Integer)

arbitraryNumberB :: GenBuilder Integer
arbitraryNumberB = lift arbitraryNumber

arbitraryPosition :: Gen Position
arbitraryPosition = (,) <$> arbitraryNumber <*> arbitraryNumber


----------------------------------------------------------------------
--- Formatting
----------------------------------------------------------------------

spaceSep :: [String] -> Gen String
spaceSep ss = foldl (<++>) (return "") $ zipWith spaceSep' ss' $ tail ss'
  where ss' = "" : ss ++ [""]
        spaceSep' s0 s1
          | start0 s0 || start0 s1 = return s0 <++> spaces 0
          | otherwise = return s0 <++> spaces 1
          where start0 s | null s = False
                         | otherwise = last s `elem` "(){}[]@|,."

spaces :: Int -> Gen String
spaces start = do
  n <- choose (start, 1)
  replicateM n (elements " \t\n")

maybeBetween :: String -> String -> Gen String -> Gen String
maybeBetween a b g = oneof [g, f]
  where f = do
          g' <- g
          spaceSep [a, g', b]

showProgram :: Program -> Gen String
showProgram p = spaceSep =<< mapM showDefCom p

showDefCom :: DefCom -> Gen String
showDefCom dc = case dc of
  Def d -> showDefinition d
  Com c -> showCommand c

showDefinition :: Definition -> Gen String
showDefinition d = case d of
  Viewdef i e0 e1 -> do
    e0' <- showExpr e0
    e1' <- showExpr e1
    spaceSep ["viewdef", i, e0', e1']
  Rectangle i e0 e1 e2 e3 col -> do
    [e0', e1', e2', e3'] <- mapM showExpr [e0, e1, e2, e3]
    spaceSep ["rectangle", i, e0', e1', e2', e3', showColour col]
  Circle i e0 e1 e2 col -> do
    [e0', e1', e2'] <- mapM showExpr [e0, e1, e2]
    spaceSep ["circle", i, e0', e1', e2', showColour col]
  View i -> spaceSep ["view", i]
  Group gi vis -> spaceSep (["group", gi, "["] ++ vis ++ ["]"])

showCommand :: Command -> Gen String
showCommand c = case c of
  Move sis p -> do
    p' <- showPos p
    spaceSep (sis ++ ["->", p'])
  At c1 i -> do
    c1' <- showCommand c1
    c1'' <- case c1 of
      Par _ _ -> spaceSep ["{", c1', "}"]
      _ -> return c1'
    spaceSep [c1'', "@", i]
  Par c0 c1 -> do
    c0' <- showCommand c0
    c1' <- showCommand c1
    spaceSep [c0', "||", c1']

showPos :: Pos -> Gen String
showPos p = case p of
  Abs e0 e1 -> do
    e0' <- showExpr e0
    e1' <- showExpr e1
    spaceSep ["(", e0', ",", e1', ")"]
  Rel e0 e1 -> do
    e0' <- showExpr e0
    e1' <- showExpr e1
    spaceSep ["+", "(", e0', ",", e1', ")"]

showExpr :: Expr -> Gen String
showExpr e = maybeBetween "(" ")" $ case e of
  Plus e0 e1 -> do
    e0' <- showExpr e0
    e1' <- showExpr e1
    spaceSep [e0', "+", e1']
  Minus e0 e1 -> do
    e0' <- showExpr e0
    e1' <- showExpr e1
    let e1'' = case e1 of
          Plus _ _ -> ["(", e1', ")"]
          Minus _ _ -> ["(", e1', ")"]
          _ -> [e1']
    spaceSep ([e0', "-"] ++ e1'')
  Const n -> spaceSep [show n]
  Xproj i -> spaceSep [i, ".", "x"]
  Yproj i -> spaceSep [i, ".", "y"]


showColour :: Colour -> String
showColour = getColourName
