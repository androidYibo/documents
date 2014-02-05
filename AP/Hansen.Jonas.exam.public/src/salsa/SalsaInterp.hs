--
-- Implementation of Salsa interpreter
--
-- By Jonas Stig Kaempf Hansen
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp
  (Position, interpolate, runProg, runTests)
where

-- Test dependencies. Test cases are at the end
import Test.HUnit
import Test.QuickCheck
  ( arbitrary, choose, vectorOf, oneof, elements
  , Gen, Arbitrary, quickCheckWith, stdArgs, maxSuccess)
import qualified Test.QuickCheck as QC

-- API implementation dependencies
import SalsaAst
import Gpx

import Control.Monad(liftM, liftM2, liftM3)
import Data.Maybe(fromMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (sort, transpose)
import qualified Data.List as List

--
-- Interpolate move from p1 to pn in n steps
--
type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
interpolate n p1 pn 
  | n < 0  = error $ "Error! Tried to interpolate with n = " ++ show n
  | n == 0 = [] -- Specification says that list has n elements!
  | otherwise = [pos i | i <- steps]
  where
    (x1, y1) = p1
    (xn, yn) = pn
    (dx, dy) = (xn - x1, yn - y1) -- distance to move in total
    steps = [1..n]
        -- excludes the initial pos (p1) from steps;
        -- use [0..n] to include
    pos :: Integer -> Position
    pos i = (x1 + round dxi, y1 + round dyi)
      where
        n' :: Double
        n' = fromInteger n
        (dxi, dyi) = (fromInteger (i * dx) / n', fromInteger (i * dy) / n')

--
-- Types Context and SalsaCommand
--

data Shape = Rect Integer Integer String
           | Cir  Integer String
     deriving (Show, Eq)
type ShapeDefs = Map Ident Shape
data ViewType = Vw Integer Integer | Gr [Ident] deriving (Show, Eq)
type ViewDefs = Map Ident ViewType
type Defs = (ViewDefs, ShapeDefs) 

type ActiveViews = Set Ident
type FrameRate = Integer
type Environment = (Defs, ActiveViews, FrameRate)

type ShapePos = [(Ident, Position)] -- should properly be sets
type Shapes = Map Ident ShapePos
data Context = Context Environment Shapes deriving (Show, Eq)

newtype SalsaCommand a = SalsaCommand { runSC :: Context -> (a, Context) }
instance Monad SalsaCommand where
  return x  = SalsaCommand $ \c -> (x, c)
  m >>= f   = SalsaCommand $ \c@(Context env _) ->
                let (x, Context _ shapes1) = runSC m c
                in runSC (f x) (Context env shapes1)

--
-- functions for manipulating the context
--

-- * Context initializers

emptyEnvironment :: FrameRate -> Environment
emptyEnvironment n = (emptyDefs, Set.empty, n)
  where emptyDefs = (Map.empty, Map.empty)

emptyContext :: FrameRate -> Context
emptyContext n = Context (emptyEnvironment n) Map.empty

-- * Context lookups

askContext :: SalsaCommand Context
askContext = SalsaCommand $ \c -> (c, c)

askEnv :: SalsaCommand Environment
askEnv = do (Context env _) <- askContext
            return env

askShapes :: SalsaCommand Shapes
askShapes = do (Context _ ss) <- askContext
               return ss
            
lookupActive :: SalsaCommand ActiveViews
lookupActive = do (_, av, _) <- askEnv
                  return av

lookupShapePos :: Ident -> SalsaCommand (Maybe ShapePos)
lookupShapePos sid = do (Context _ ss) <- askContext
                        return $ Map.lookup sid ss
     
lookupViewDef :: Ident -> SalsaCommand (Maybe ViewType)
lookupViewDef vid = do vDefs <- getViewDefs
                       return $ Map.lookup vid vDefs

lookupDefs :: Environment -> SalsaCommand Defs
lookupDefs (defs, _, _) = return defs

-- * Getters

getShapes :: SalsaCommand Shapes
getShapes = askShapes
                
getDefs :: SalsaCommand Defs
getDefs = lookupDefs =<< askEnv
             
getShapeDefs :: SalsaCommand ShapeDefs
getShapeDefs = liftM snd getDefs
              
getViewDefs :: SalsaCommand ViewDefs
getViewDefs = liftM fst getDefs

getActive :: SalsaCommand [Ident]
getActive = do av <- lookupActive
               return $ Set.toList av
                 
-- * Local context mutation
     
-- Allows for local changes to context, but any changes will simply
-- be disregarded after the SalsaCommand has executed
local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a
local f m = SalsaCommand $ \c -> let c' = f c
                                 in runSC m c'


-- * Context mutators

insertShapeDef :: Ident -> Shape -> Environment -> Environment
insertShapeDef sid shape ((vDefs, sDefs), a, f) = 
  ((vDefs, Map.insert sid shape sDefs), a, f)

insertShapePos :: Ident -> ShapePos -> Shapes -> Shapes
insertShapePos = Map.insert

insertViewDef :: Ident -> ViewType -> Environment -> Environment
insertViewDef vid view ((vDefs, sDefs), a, f) = 
  ((Map.insert vid view vDefs, sDefs), a, f)

makeActive :: [Ident] -> Context -> Context
makeActive vids (Context (defs, _, fr) ss) = 
  Context (defs, Set.fromList vids, fr) ss
  
addActive :: Ident -> Context -> Context
addActive vid (Context (defs, a, fr) ss) = 
  Context (defs, Set.insert vid a, fr) ss
  
-- * Environment updaters

addViewDef :: Ident -> ViewType -> Salsa ()
addViewDef vid vDef = 
  Salsa $ \(Context env shapes) ->
    let env' = insertViewDef vid vDef env
    in ((), Context env' shapes)
    
addShapeDef :: Ident -> Position -> Shape -> Salsa ()
addShapeDef sid pos sDef =
  Salsa $ \(Context env shapes) ->
    let env' = insertShapeDef sid sDef env
        (_,av,_) = env'
        shapePs = [(vid, pos) | vid <- Set.toList av]
        shapes' = insertShapePos sid shapePs shapes
    in ((), Context env' shapes')

clearActive :: Salsa()
clearActive = Salsa $ \c -> ((), makeActive [] c)

addView :: Ident -> Salsa ()
addView vid = Salsa $ \c -> ((), addActive vid c)
    
setActiveView :: Ident -> Salsa ()
setActiveView vid = do
  clearActive
  activate [vid]
  where
    activate :: [Ident] -> Salsa ()
    activate [] = return ()
    activate (v:vs) = do      
      vDef <- liftC $ lookupViewDef v
      case vDef of
        Nothing -> 
          error $ "Error: Undefined view " ++ show vid ++ " cannot be activated"
        Just (Vw _ _) -> do
          addView v
          activate vs
        Just (Gr gvids) ->
          activate $ gvids ++ vs
          
         
-- * Shape positions updater

setShapePos :: Ident -> ShapePos -> SalsaCommand ()
setShapePos sid ps = SalsaCommand $ \(Context env ss) -> 
  ((), Context env $ insertShapePos sid ps ss)
  
--
-- Expressions
--

lowestCoords :: Ident -> SalsaCommand (Integer, Integer)
lowestCoords sid = do
  vps <- lookupShapePos sid
  case vps of
    Nothing ->
      error $ "Error: Unknow shape (projection failed): " ++ show sid
    Just ps ->
      case sort' $ unzip $ map snd ps of
        (x:_, y:_) -> return (x, y)
        _ ->
          error $ "Error: No position (projection failed): " ++ show sid
  where sort' (a,b) = (sort a, sort b)
  
expr :: Expr -> SalsaCommand Integer
expr (Plus e1 e2)   = liftM2 (+) (expr e1) (expr e2)
expr (Minus e1 e2)  = liftM2 (-) (expr e1) (expr e2)
expr (Const n)      = return n
expr (Xproj sid)    = liftM fst $ lowestCoords sid
expr (Yproj sid)    = liftM snd $ lowestCoords sid
  
--
-- Define the function command
--

command :: Command -> SalsaCommand ()
command (Move []  _) = return ()
command (Move (sid:sids) pos) = do
    activeViews <- getActive
    ps <- lookupShapePos sid
    ps' <- case ps of
      Nothing -> error $ "Error: Tried to move unknown shape: " ++ show sid
      Just xs -> do
        let q (vid, _) = vid `elem` activeViews
        let (xs1, other) = List.partition q xs
        xs2 <- mapM updatePos xs1
        return $ xs2 ++ other

    -- update context and do remaining sids
    setShapePos sid ps'
    command (Move sids pos)
    
    where
      updatePos :: (Ident, Position) -> SalsaCommand (Ident, Position)
      updatePos (vid, (x, y)) = case pos of
        (Abs e1 e2) -> do
          (x1, y1) <- doExpr e1 e2
          return (vid, (x1, y1))
        (Rel e1 e2) -> do
          (x1, y1) <- doExpr e1 e2
          return (vid, (x + x1, y + y1))
      doExpr e1 e2 = liftM2 (,) (expr e1) (expr e2)
        
command (At c vid) = 
    local (makeActive [vid]) $ command c

command (Par c1 c2) = do
    -- this is "parallel" in as much as both commands gets to
    -- execute before next key frame is capturered.
    -- No check for illegal commands (e.g. overlapping manipulations)
    command c1
    command c2        


--
-- Define the type Salsa
--

newtype Salsa a = Salsa { runS :: Context -> (a, Context) }
instance Monad Salsa where
  return x  = Salsa $ \c -> (x, c)
  m >>= f   = Salsa $ \c ->
                let (x, c') = runS m c
                in runS (f x) c'

--
-- Define the functions liftC, definition, and defCom
--

liftC :: SalsaCommand a -> Salsa a
liftC (SalsaCommand sc) = Salsa $ \c@(Context env _) ->
  let (x, Context _ ss) = sc c
  in (x, Context env ss)

colour :: Colour -> String
colour Blue = "blue"
colour Plum = "plum"
colour Red = "red"
colour Green = "green"
colour Orange = "orange"

definition :: Definition -> Salsa ()
definition (Viewdef vid e1 e2) = do
  (w, h) <- liftC $ liftM2 (,) (expr e1) (expr e2)
  addViewDef vid $ Vw w h
  setActiveView vid
  
definition (Rectangle sid e1 e2 e3 e4 col) = do
  (x, y) <- liftC $ liftM2 (,) (expr e1) (expr e2)
  (w, h) <- liftC $ liftM2 (,) (expr e3) (expr e4)
  let c = colour col
  addShapeDef sid (x,y) $ Rect w h c
  
definition (Circle sid e1 e2 e3 col) = do
  (x, y) <- liftC $ liftM2 (,) (expr e1) (expr e2)
  r <- liftC $ expr e3
  let c = colour col
  addShapeDef sid (x,y) $ Cir r c
  
definition (View vid) = setActiveView vid
definition (Group gvid vids) = do
  addViewDef gvid $ Gr vids
  setActiveView gvid

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com cmd) = liftC $ command cmd

-- Animate the n frames from key frame (ss0) to key frame (ss1);
-- includes both starting and end key frame. n > 0
animate :: Integer -> Shapes -> Shapes -> ShapeDefs -> [Frame]
animate n ss0 ss1 defs =
  map concat $ transpose [frames sid | sid <- sids]
  {- 
    by list comprehension, 
    we get 3 times n frames for drawing shapes x, y, z (in all views):
      [[xFrame_1, xFrame_2, ..., xFrame_n]
      ,[yFrame_1, yFrame_2, ..., yFrame_n]
      ,[zFrame_1, yFrame_2, ..., yFrame_n]]
    we need n frames:
      [xFrame_1 ++ yFrame_1 ++ zFrame_1
      ,xFrame_2 ++ yFrame_2 ++ zFrame_2
      ,...
      ,xFrame_n ++ yFrame_n ++ zFrame_n]
  -}   
  where
    -- The shapes to be drawn, i.e. the ones defined in the end key frame
    sids :: [Ident] 
    sids = Map.keys ss1
  
    -- Construct animation between ss0 and ss1 for sid, i.e. construct
    --    [xFrame_1, xFrame_2, ..., xFrame_n]
    -- for shape x. Each frame draws shape x in all applicable views
    frames :: Ident -> [Frame]
    frames sid =
      let
        -- we neeed instructions for drawing shape sid in each 
        -- of these views from pos0 to pos1, for each view
        views =   
          [(vid1, pos0, pos1) 
           | (vid1, pos1) <- ps1
           , (vid0, pos0) <- ps0
           , vid0 == vid1]
        instrs = [drawInstrs shapeT p | p <- views]
      in 
        case instrs of
          -- if shape is defined only in end key frame, we have no instrs 
          -- until last frame
          [] -> replicate (fromInteger n - 1) [] ++
            [[appearInstr shapeT (vid1,pos1) | (vid1,pos1) <- ps1]]
          
          {-else we get 2 x n instr for drawing shape x in views A and B:
              instrs = [[xA_1, xA_2, ..., xA_end], [xB_1, xB_2, ..., xB_end]] 
            we need n frames:
              frames = [[xA_1, xB_1], [xA_2, xB_2], ..., [xA_end, xB_end]] -}
          xs -> transpose xs
      
      where 
        -- make n instrs for drawing shape sid in view vid from pos0 to pos1
        drawInstrs :: Shape -> (Ident, Position, Position) -> [GpxInstr]
        drawInstrs (Rect w h c) (vid, pos0, pos1) =
          [DrawRect x y w h vid c | (x, y) <- shapeSteps pos0 pos1]
        drawInstrs (Cir r c) (vid, pos0, pos1) =
          [DrawCirc x y r vid c | (x, y) <- shapeSteps pos0 pos1]
        
        -- make 1 instr for drawing shape sid in view vid at pos1
        appearInstr :: Shape -> (Ident, Position) -> GpxInstr
        appearInstr (Rect w h c) (vid,(x,y)) = DrawRect x y w h vid c
        appearInstr (Cir r c) (vid,(x,y)) = DrawCirc x y r vid c
        
        shapeSteps = interpolate n
    
        ps1 = fromMaybe (error "Error: Shape was not there!")
                        $ Map.lookup sid ss1
        ps0 = fromMaybe []  -- in this case, the shape was defined 
                            -- after the command, and shows only 
                            -- in the end key frame
                        $ Map.lookup sid ss0
          
        shapeT = fromMaybe 
                  (error $ "Error: Shape definition missing: " ++ show sid)
                  $ Map.lookup sid defs
      
--
-- Define the function runProg
--

data KeyFrame = NoKeyFrame | KeyFrame Shapes

runProg :: Integer -> Program -> Animation
runProg n p 
  | n < 1 = 
      error "Error: How should I animate anything with that frame rate?"
  | otherwise = fst $ runS (run p NoKeyFrame) (emptyContext n)
  
  where
    -- run program starting from key frame key0
    run :: Program -> KeyFrame -> Salsa Animation
    run [] NoKeyFrame = return ([], [])
    run [] (KeyFrame key0) = do
      key1 <- liftC getShapes -- capture final key frame
      sDefs <- liftC getShapeDefs
      let fs = animate n key0 key1 sDefs
      defs <- finalDefs
      return (defs, fs)
    
    run (Def def:dcs) kf = do
      defCom (Def def)
      (defs, fss) <- run dcs kf
      return (defs, fss)
          
    -- Commands indicate transitions between key frames
    run (Com cmd:dcs) kf = do
      sDefs <- liftC getShapeDefs
      curKey <- liftC getShapes
      let (n', key0, key1) = 
            case kf of
              -- initial key frame is now fully defined; draw it
              NoKeyFrame -> (1, curKey, curKey)
              -- some prev key frame already drawn; draw from that
              KeyFrame k0 -> (n, k0, curKey)
      let fs = animate n' key0 key1 sDefs
      defCom (Com cmd)
      (defs, fss) <- run dcs (KeyFrame key1)
      return (defs, fs ++ fss)
    
    finalDefs :: Salsa [(ViewName, Integer, Integer)]
    finalDefs = do
      defs <- liftC getViewDefs
      return [(vid, w, h) | (vid, Vw w h) <- Map.toList defs]
      
      

-- ============================================================================
--                                  TESTS
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- 1. QuickCheck for interpolate
-------------------------------------------------------------------------------
   
prop_pLength :: QC.NonNegative Integer -> Position -> Position -> Bool
prop_pLength (QC.NonNegative n) p1 p2 =
  length (interpolate n p1 p2) == fromInteger n

-- Only test non-empty lists
prop_pEndAtP2 :: QC.Positive Integer -> Position -> Position -> Bool
prop_pEndAtP2 (QC.Positive n) p1 p2 =
  last (interpolate n p1 p2) == p2
  
-- We must have that each list of coordinates move in same direction always
-- Tests only non-empty lists
prop_pOrdered :: QC.Positive Integer -> Position -> Position -> Bool
prop_pOrdered (QC.Positive n) p1 p2 =
  let ps = interpolate n p1 p2
      (xs, ys) = unzip ps
  in (isSorted xs || isSorted (reverse xs)) 
      && (isSorted ys || isSorted (reverse ys))
  where
    isSorted [] = True
    isSorted (z:zs) = fst $ foldl f (True, z) zs
    f (acc, x0) x1 = (acc && (x0 <= x1), x1)
    
-- Distances between points are about the same, i.e. ~= an n'th of the total
-- distance. Tests only non-empty lists
prop_pDistance :: QC.Positive Integer -> Position -> Position -> Bool
prop_pDistance (QC.Positive n) p_start p_end =
  let ps = p_start : interpolate n p_start p_end
      (dx', dy') = distance p_start p_end
      n' = fromInteger n
      d = (dx' / n', dy' / n') -- = the step in each coord
  in assertStepDistance d ps
  where
    -- Returns absolute distance split into dx and dy
    distance :: Position -> Position -> (Double, Double)
    distance (x1, y1) (x2, y2) = 
      (fromInteger $ abs $ x2 - x1, fromInteger $ abs $ y2 - y1)
            
    assertStepDistance :: (Double, Double) -> [Position] -> Bool
    assertStepDistance _ [] = True
    assertStepDistance _ [_] = True
    assertStepDistance (dx, dy) (p1:p2:pss) = 
      diffInBounds && assertStepDistance (dx, dy) (p2:pss)
      where
        (dx_i, dy_i) = distance p1 p2
        diffInBounds = (dx - 1.0 <= dx_i && dx_i <= dx + 1.0) && 
                       (dy - 1.0 <= dy_i && dy_i <= dy + 1.0)

runInterpolateTests :: IO ()
runInterpolateTests = do
  quickCheckWith stdArgs {maxSuccess = 10000} prop_pLength
  quickCheckWith stdArgs {maxSuccess = 10000} prop_pEndAtP2
  quickCheckWith stdArgs {maxSuccess = 10000} prop_pOrdered
  quickCheckWith stdArgs {maxSuccess = 10000} prop_pDistance


-------------------------------------------------------------------------------
-- 2. Unit tests for command
-------------------------------------------------------------------------------

tShape1orig :: (Ident, ShapePos)
tShape1orig = ("x", [("A", (50, 70))]) 
tShape2orig :: (Ident, ShapePos)
tShape2orig = ("y", [("A", (20, 200)), ("B", (65, 30))]) 
  
tCtxt :: Context
tCtxt =
 let
  -- Initial context for command unit test cases
  tViewDefs = Map.fromList [("A", Vw 300 300), ("B", Vw 600 400)]
  tShapeDefs = Map.fromList [("x", Cir 10 "blue"), ("y", Rect 25 42 "plum")]
  tActive = Set.fromList ["A"]
  tEnv = ((tViewDefs, tShapeDefs), tActive, 10)
  
  tShapes = Map.fromList [tShape1orig, tShape2orig]
 in Context tEnv tShapes
 
-- Command test cases
tCommands :: [Test]
tCommands = 
 let
  (Context tEnv@((tViewDefs, tShapeDefs), _, _) _) = tCtxt
  
  -- Test on equality of contexts before and after single command
  cmdTest name inC outC testCmd =
    cmdTest' name inC outC (command testCmd)
    
  -- Test on equality of contexts before and after chain of commands
  cmdTest' name inC outC testCmd =
    TestCase $ assertEqual name (snd $ runSC testCmd inC) outC

  -- Test 1 - Absolute move on active view
  tShape1_move1 = ("x", [("A", (20, 20))])
  tCmd1 = cmdTest "Absolute move. Active view" 
    tCtxt (Context tEnv $ Map.fromList [tShape1_move1, tShape2orig])
    $ Move ["x"] (Abs (Const 20) (Const 20))

  -- Test 2 - Relative move on active view
  tShape1_move2 = ("x", [("A", (70, 70))])
  tCmd2 = cmdTest "Relative move. Active view" 
    tCtxt (Context tEnv $ Map.fromList [tShape1_move2, tShape2orig])
    $ Move ["x"] (Rel (Const 20) (Const 0))

  -- Test 3 - Move more than one
  tShape1_move3 = ("x", [("A", (100, 75))])
  tShape2_move1 = ("y", [("A", (70, 205)), ("B", (65, 30))])
  tCmd3 = cmdTest "Relative move of two shapes. Active view"
    tCtxt (Context tEnv $ Map.fromList [tShape1_move3, tShape2_move1])
    $ Move ["x", "y"] (Rel (Const 50) (Const 5))
    
  -- Test 3a - Move one shape in two active views
  tShape1_move3a = ("x", [("A", (50, 70))])
  tShape2_move1a = ("y", [("A", (70, 205)), ("B", (115, 35))])
  tCtxt3a@(Context tEnv3a _) = makeActive ["A", "B"] tCtxt
  tCmd3a = cmdTest "Relative move of one shape in two active views"
    tCtxt3a
    (Context tEnv3a $ Map.fromList [tShape1_move3a, tShape2_move1a])
    $ Move ["y"] (Rel (Const 50) (Const 5))
    
  -- Test 4 - Move in non-active view, followed by move in active 
  tShape2_move2 = ("y", [("A", (42, 42)), ("B", (42, 42))])
  tCmd4_0 = do _ <- command $ At (Move ["y"] (Abs (Const 42) (Const 42))) "B"
               command $ Move ["y"] (Abs (Const 42) (Const 42))
  tCmd4 = cmdTest' "Relative move. Non-Active view"
    tCtxt (Context tEnv $ Map.fromList [tShape1orig, tShape2_move2])
    $tCmd4_0

  -- Test 5 - Chained '@'s; ignore all but first
  tShape2_move3 = ("y", [("B", (42, 42)), ("A", (20, 200))])
  tCmd5_0 = At (At (Move ["y"] (Abs (Const 42) (Const 42))) "B") "A"
  tActive5 = Set.fromList ["B"] -- This test tentatively changes active view
  tEnv5 = ((tViewDefs, tShapeDefs), tActive5, 10)
  tCmd5 = cmdTest
    "Abs move in non-active view; then chain of @s, no effect."
    tCtxt (Context tEnv5 $ Map.fromList [tShape1orig, tShape2_move3])
    $tCmd5_0
    
  -- Test 6 - Parallel commands
  tShape1_move4 = ("x", [("A", (30, 30))])
  tShape2_move4 = ("y", [("B", (42, 42)), ("A", (20, 200))])
  tCmd6_0 = Par (At (Move ["y"] (Abs (Const 42) (Const 42))) "B") 
                (Move ["x"] (Abs (Const 30) (Const 30)))
  tCmd6 = cmdTest
    "Par cmd with move in non-active view and move in active view"
    tCtxt (Context tEnv $ Map.fromList [tShape1_move4, tShape2_move4])
    $tCmd6_0
    
  -- Advanced context for command unit test cases
  tViewDefs1 = Map.fromList [("A", Vw 300 300), ("B", Vw 600 400)
                            ,("C", Vw 500 200)]
  tShapeDefs1 = Map.fromList [("x", Cir 10 "blue")
                             ,("y", Rect 25 42 "plum")
                             ,("z", Cir 65 "orange")]
  tActive6 = Set.fromList ["A", "C"]
  tEnv6 = ((tViewDefs1, tShapeDefs1), tActive6, 10)
  tShape3_orig = ("z", [("B", (777, 888)), ("C", (1, 1))]) 
  tShapes1 = Map.fromList [tShape1orig, tShape2orig, tShape3_orig]
  tCtxt1 = Context tEnv6 tShapes1
    
  -- Test 7 - More parallel commands
  tShape1_move5 = ("x", [("A", (51, 71))])
  tShape3_move1 = ("z", [("C", (456, 789)), ("B", (999, 666))])
  tCmd7_0 = Par (Par (Move ["x"] (Rel (Const 1) (Const 1)))
                     (At (Move ["z"] (Abs (Const 999) (Const 666))) "B"))
                (Move ["z"] (Abs (Const 456) (Const 789)))
  tCmd7 = cmdTest
    "Par with nested par in active and non-active views"
    tCtxt1 (Context tEnv6 $ Map.fromList [tShape1_move5
                                         ,tShape2orig
                                         ,tShape3_move1])
    $tCmd7_0
    
 in
  [tCmd1
  ,tCmd2
  ,tCmd3
  ,tCmd3a
  ,tCmd4
  ,tCmd5
  ,tCmd6
  ,tCmd7
  ]
-------------------------------------------------------------------------------
-- 3. Unit tests for expr
-------------------------------------------------------------------------------

tExpressions :: [Test]
tExpressions =
 let
  -- Initial context for expression unit test cases (same as for commands)
  
  -- Test on expecting value of expression
  exprTest name inC outVal tExpr =
    TestCase $ assertEqual name (fst $ runSC (expr tExpr) inC) outVal

  -- Plus, minus and Const cases are trivial
  tExpr1 = exprTest "Constant" tCtxt 42 $ Const 42
  tExpr2 = exprTest "Plus constants" tCtxt 42 
           $ Plus (Const 21) (Const 21)
  tExpr3 = exprTest "Plus constants" tCtxt 42 
           $ Minus (Const 84) (Const 42)
  -- Projections
  tExpr4 = exprTest "Xproj with shape defined in one view"
            tCtxt 50 $ Xproj "x"
  tExpr5 = exprTest "Xproj with shape defined in more views"
            tCtxt 20 $ Xproj "y"
  tExpr6 = exprTest "Yproj with shape defined in one view"
            tCtxt 70 $ Yproj "x"
  tExpr7 = exprTest "Yproj with shape defined in more views"
            tCtxt 30 $ Yproj "y"
 in
  [tExpr1
  ,tExpr2
  ,tExpr3
  ,tExpr4
  ,tExpr5
  ,tExpr6
  ,tExpr7
  ]

-------------------------------------------------------------------------------
-- 4. Unit tests for definition
-------------------------------------------------------------------------------

-- Definition test cases
tDefinitions :: [Test]
tDefinitions = 
 let
  -- Initial context for definition unit test cases
  tCtxt' = emptyContext 10
  (Context tEnv tShapes) = tCtxt'
  
  setAtv  a     ((v,s),_,fr)  = ((v,s),Set.fromList a,fr)
  setVDef v     ((_,s),a,fr)  = ((Map.fromList v,s),a,fr)
  addVDef (k,v) ((vs,s),a,fr) = ((Map.insert k v vs,s),a,fr)
  -- setSDef s     ((v,_),a,fr)  = ((v,Map.fromList s),a,fr)
  addSDef (k,s) ((v,ss),a,fr) = ((v,Map.insert k s ss),a,fr)
  
  addSPos (k,ps)              = Map.insert k ps

  -- Test on equality of contexts; single definition
  defTest name inC outC testDef =
    defTest' name inC outC (definition testDef)
    
  -- Test on equality of contexts; more definitions
  defTest' name inC outC testDef =
    TestCase $ assertEqual name outC $ snd $ runS testDef inC

  -- Test 1 - View definition in empty environment
  tEnv1 = setAtv ["A"] $ setVDef [("A", Vw 200 100)] tEnv
  tDef1 = defTest "Viewdef in empty context" 
    tCtxt' (Context tEnv1 tShapes)
    $ Viewdef "A" (Const 200) (Const 100)

  -- Test 2 - View definition in non-empty environment
  tEnv2 = setAtv ["B"] $ 
          setVDef [("B", Vw 500 300), ("A", Vw 200 100)] tEnv
  tDef2 = defTest "Viewdef in non-empty context" 
    (Context tEnv1 tShapes) (Context tEnv2 tShapes)
    $ Viewdef "B" (Const 500) (Const 300)
    
  -- Test 3 - Making single view active when already active
  tDef3 = defTest "Single view active when already active" 
    (Context tEnv2 tShapes) (Context tEnv2 tShapes)
    $ View "B"
    
  -- Test 4 - Making single view active when non-active
  tEnv4 = setAtv ["A"] tEnv2
  tDef4 = defTest "Single view active when non-active" 
    (Context tEnv2 tShapes) (Context tEnv4 tShapes)
    $ View "A"
    
  -- Test 5 - Defining group of all previously defined views
  tEnv5 = setAtv ["A", "B"] $ 
          addVDef ("X", Gr ["A", "B"]) tEnv4
  tDef5 = defTest "Group of two views" 
    (Context tEnv4 tShapes) (Context tEnv5 tShapes)
    $ Group "X" ["A", "B"]
    
  -- Test 6 - Defining group of one previously defined views
  tEnv6 = setAtv ["B"] $ 
          addVDef ("Y", Gr ["B"]) tEnv5
  tDef6 = defTest "Group of one" 
    (Context tEnv5 tShapes) (Context tEnv6 tShapes)
    $ Group "Y" ["B"]
    
  -- Test 7 - Chaining defitions, context updates
  tEnv7 = setAtv ["D"] $ 
          addVDef ("C", Vw 1000 4200) $
          addVDef ("D", Vw 0 0) tEnv6
  tDef7 = defTest' "First viewdef, then another" 
    (Context tEnv6 tShapes) (Context tEnv7 tShapes)
    (do definition $ Viewdef "C" (Const 1000) (Const 4200)
        definition $ Viewdef "D" (Const 0) (Const 0))

  -- Test 8 - Defining group consisting of view and other group
  tEnv8 = setAtv ["A", "B", "C"] $ 
          addVDef ("Z", Gr ["X", "C"]) tEnv7
  tDef8 = defTest "Group of one view, one other group" 
    (Context tEnv7 tShapes) (Context tEnv8 tShapes)
    $ Group "Z" ["X", "C"]
    
  -- Test 9 - Defining group with duplicates
  tEnv9 = setAtv ["A", "B"] $ 
          addVDef ("W", Gr ["X", "Y"]) tEnv8
  tDef9 = defTest "Group of two groups, with duplicates" 
    (Context tEnv8 tShapes) (Context tEnv9 tShapes)
    $ Group "W" ["X", "Y"]
    
  -- Test 10 - Groups of groups of groups
  tEnv10 = setAtv ["A", "C", "B", "D"] $ 
           addVDef ("V", Gr ["D", "Z"]) tEnv9
  tDef10 = defTest "Group of groups of groups" 
    (Context tEnv9 tShapes) (Context tEnv10 tShapes)
    $ Group "V" ["D", "Z"]
    
  -- Test 11 - Rectangle in one active view
  tEnv11 = addSDef ("s", Rect 100 200 "blue") tEnv7
  tShapes11 = addSPos ("s", [("D", (1,2))]) tShapes
  tDef11 = defTest "Rectangle, one active view" 
    (Context tEnv7 tShapes) (Context tEnv11 tShapes11)
    $ Rectangle "s" (Const 1) (Const 2) (Const 100) (Const 200) Blue

  -- Test 12 - Circle in one active view
  tEnv12 = addSDef ("t", Cir 42 "plum") tEnv11
  tShapes12 = addSPos ("t", [("D", (3,4))]) tShapes11
  tDef12 = defTest "Circle, one active view" 
    (Context tEnv11 tShapes11) (Context tEnv12 tShapes12)
    $ Circle "t" (Const 3) (Const 4) (Const 42) Plum
    
  -- Test 13 - Rectangle in two active views
  tEnv13 = addSDef ("s", Rect 100 200 "blue") tEnv9
  tShapes13 = addSPos ("s", [("A", (1,2))
                            ,("B", (1,2))]) tShapes
  tDef13 = defTest "Rectangle, two active views" 
    (Context tEnv9 tShapes) (Context tEnv13 tShapes13)
    $ Rectangle "s" (Const 1) (Const 2) (Const 100) (Const 200) Blue
    
 in
  [tDef1
  ,tDef2
  ,tDef3
  ,tDef4
  ,tDef5
  ,tDef6
  ,tDef7
  ,tDef8
  ,tDef9
  ,tDef10
  ,tDef11
  ,tDef12
  ,tDef13
  ]

-------------------------------------------------------------------------------
-- 5. Unit tests runs
-------------------------------------------------------------------------------
tests :: Test
tests = TestList
  [TestLabel "Commands"    $ TestList tCommands
  ,TestLabel "Expressions" $ TestList tExpressions
  ,TestLabel "Definitions" $ TestList tDefinitions
  ]

runHUTests :: IO Counts
runHUTests = runTestTT tests


-------------------------------------------------------------------------------
-- 6. QuickCheck for animate
-------------------------------------------------------------------------------

-- tProg :: Program
-- tProg = [ Def (Viewdef "One" (Const 500) (Const 500))
        -- , Def (Viewdef "Two" (Const 400) (Const 400))
        -- , Def (Group "Both" ["One","Two"])
        -- , Def (View "Both")
        -- , Def (Rectangle "larry" (Const 10) (Const 350)
                                 -- (Const 20) (Const 20) Blue)
        -- , Def (Rectangle "fawn" (Const 300) (Const 350)
                                -- (Const 15) (Const 25) Plum)
        -- , Def (View "Two")
        -- , Com (Par (Move ["larry"] (Abs (Const 300) (Const 350)))
                   -- (Move ["fawn"] (Abs (Const 10) (Const 350))))
        -- , Def (View "Both")
        -- , Com (Move ["larry","fawn"]
                    -- (Rel (Const 0) (Minus (Const 0) (Const 300))))]

-- We test only for limited number of views, shapes, since the algorithm
-- should be correct for larger numbers as well, if it is for smaller
      
-- Max length of generated identifiers (we remove duplicates before use)
identSize :: Int
identSize = 5
 
videntChars :: String 
videntChars = ['A'..'Z']
sidentChars :: String
sidentChars = ['a'..'z']

getIdent :: String -> Gen Ident
getIdent chars = vectorOf identSize $ QC.elements chars

getIdents :: String -> Gen [Ident]
getIdents chars = do
  n <- choose (1, 100)
  vids <- vectorOf n $ getIdent chars
  return $ List.nub vids -- remove dublicates, but still at least 1
  
newtype TestPos = TestPos Position
instance Arbitrary TestPos where
  arbitrary = liftM TestPos $ liftM2 (,) getn getn
    where getn = do { (QC.NonNegative n) <- arbitrary; return n }
        
getShape :: [Ident] -> Gen ((Ident, ShapePos)
                           ,(Ident, ShapePos)
                           ,(Ident, Shape))
getShape vids = do
    sid <- getIdent sidentChars
    
    def'ed_on <- subsequence vids
    ps0 <- vectorOf (length def'ed_on) arbitrary
    ps1 <- vectorOf (length def'ed_on) arbitrary
    let pss0 = zip def'ed_on [p | (TestPos p) <- ps0]
    let pss1 = zip def'ed_on [p | (TestPos p) <- ps1]
    
    shape <- oneof [aRect, aCir]
    
    return ((sid, pss0), (sid, pss1), (sid, shape))
    
    where
      -- can be invalid shapedefs, but serves our purpose
      aRect = liftM3 Rect arbitrary arbitrary colors
      aCir  = liftM2 Cir arbitrary colors
      colors = elements ["blue", "plum", "red", "green", "orange"]

newtype SubSeq = SubSeq ([Ident] -> Gen [Ident])
instance Arbitrary SubSeq where
  arbitrary = return $ SubSeq subsequence
  
distro :: [Integer]
distro = [0,0,1] -- 2-1 for not picking

subsequence :: [Ident] -> Gen [Ident]
subsequence [] = return [] -- Bad Thing to have happen!
subsequence (x:xs) = do
  selectBits <- vectorOf (length xs) $ elements distro
  return $ x: [x' | (1, x') <- zip selectBits xs] -- ensure at least one
       
newtype TestKeyFrames = TestKeyFrames (Shapes, Shapes, ShapeDefs)
  deriving (Eq, Show)
instance Arbitrary TestKeyFrames where
  arbitrary = do
    vids <- getIdents videntChars

    n <- choose (1, 200)
    ss <- vectorOf n $ getShape vids
    let (ss0, ss1, sdefs) = unzip3 ss

    return $ TestKeyFrames (Map.fromList ss0
                           ,Map.fromList ss1
                           ,Map.fromList sdefs)

prop_pAnimationLength :: QC.Positive Integer -> TestKeyFrames -> Bool
prop_pAnimationLength (QC.Positive n) (TestKeyFrames (ss0, ss1, defs)) =
  length (animate n ss0 ss1 defs) == fromInteger n

-- For this property, we only care about positive n
prop_pFrameLength :: QC.Positive Integer -> TestKeyFrames -> Bool
prop_pFrameLength (QC.Positive n) (TestKeyFrames (ss0, ss1, defs)) =
  let rs = animate n ss0 ss1 defs
      m = length $ concatMap snd (Map.toList ss1)
      p fr = length fr == m
  in all p rs

-- For this property, we only care about positive n
prop_pInstructions :: QC.Positive Integer -> TestKeyFrames -> Bool
prop_pInstructions (QC.Positive n) (TestKeyFrames (ss0, ss1, defs)) =
  let ss = Map.toList ss1
  in all q ss   -- forall s in S
  where
    rs = animate n ss0 ss1 defs
    
    -- filter frame for specific shape
    f :: Shape -> GpxInstr -> Bool
    f (Rect w h c) (DrawRect _ _ w' h' _ c') = w' == w && h' == h && c' == c
    f (Rect {}) _ = False
    f (Cir r c) (DrawCirc _ _ r' _ c') = r' == r && c' == c
    f (Cir {} ) _ = False
    
    p :: Shape -> Int -> Frame -> Bool
    p s m fr = m <= length (filter (f s) fr)
    
    q :: (Ident, ShapePos) -> Bool
    q (s, ps) = all (p s1 $ length ps) rs
      where
        s1 = fromMaybe (error "Test failed") (Map.lookup s defs)

runAnimateTests :: IO () 
runAnimateTests = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_pAnimationLength
  quickCheckWith stdArgs {maxSuccess = 1000} prop_pFrameLength
  quickCheckWith stdArgs {maxSuccess = 500} prop_pInstructions

-------------------------------------------------------------------------------
-- 7. Run all tests
-------------------------------------------------------------------------------

runTests :: IO Counts
runTests = do
  runInterpolateTests
  runAnimateTests
  runHUTests
