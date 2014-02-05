{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
{---------------------------------------------------------------------
Salsa interpreter.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module SalsaInterp where

import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Applicative hiding (Const)
import Control.Monad
import Control.Arrow (second)

import Misc
import SalsaAst
import Gpx


----------------------------------------------------------------------
--- Types
----------------------------------------------------------------------

type Shape = Position -> VIdent -> GpxInstr

data Env = Env { views :: [(VIdent, Integer, Integer)]
               , groups :: [(VIdent, [VIdent])]
               , shapes :: M.Map VIdent (M.Map SIdent Shape)
               , activeViews :: [VIdent]
               , envFps :: Integer
               }

-- | The positions of the shapes in the views.
type ShapePosMap = M.Map VIdent (M.Map SIdent Position)

data Context = Context { contextEnv :: Env
                       , contextSPM :: ShapePosMap
                       }

newtype SalsaCommand a = SalsaCommand {
  runSC :: Context -> (ShapePosMap, [Frame], a) }

newtype Salsa a = Salsa { runS :: Context -> (Context, [Frame], a) }


----------------------------------------------------------------------
--- Class glue for common functionality
----------------------------------------------------------------------

class SalsaSomething s where
  getContext :: s Context
  modifySPM :: (ShapePosMap -> ShapePosMap) -> s ()


----------------------------------------------------------------------
--- Instances of Monad, Functor and Applicative
----------------------------------------------------------------------

instance Monad SalsaCommand where
  return a = SalsaCommand $ \(Context _ sm) -> (sm, [], a)

  x >>= g = SalsaCommand f
    where f ctx0@(Context env _) = (sm2, frms2, b)
            where (sm1, frms0, a) = runSC x ctx0
                  ctx1 = Context env sm1
                  (sm2, frms1, b) = runSC (g a) ctx1
                  frms2 = frms0 ++ frms1

instance Functor SalsaCommand where
  fmap = liftM

instance Applicative SalsaCommand where
  pure = return
  (<*>) = ap

instance SalsaSomething SalsaCommand where
  getContext = SalsaCommand $ \ctx@(Context _ sm) -> (sm, [], ctx)
  modifySPM sf = SalsaCommand $ \(Context _ sm) -> (sf sm, [], ())


instance Monad Salsa where
  return a = Salsa $ \ctx -> (ctx, [], a)

  x >>= g = Salsa f
    where f ctx0 = (ctx2, frms2, b)
            where (ctx1, frms0, a) = runS x ctx0
                  (ctx2, frms1, b) = runS (g a) ctx1
                  frms2 = frms0 ++ frms1

instance Functor Salsa where
  fmap = liftM

instance Applicative Salsa where
  pure = return
  (<*>) = ap

instance SalsaSomething Salsa where
  getContext = Salsa $ \ctx -> (ctx, [], ctx)
  modifySPM sf = modifyContext (\(Context env sm) -> Context env (sf sm))


----------------------------------------------------------------------
--- General functions
----------------------------------------------------------------------

-- | Interpolate positions.  Exclude the start position.
interpolate :: Integer -> Position -> Position -> [Position]
interpolate 0 _ _ = []
interpolate n p0 p1 | p0 < p1 = interpolate' p0 p1
                    | otherwise = tail (reverse $ interpolate' p1 p0) ++ [p1]
  where interpolate' (startX, startY) (endX, endY) 
          = zip (makeList startX endX) (makeList startY endY)
        makeList start end = map (round . (+ fromIntegral start)
                                  . (* step) . fromIntegral) [1..n - 1]
                             ++ [end]
          where step :: Double
                step = fromIntegral (end - start) / fromIntegral n

emptyEnv :: Env
emptyEnv = Env { views = []
               , groups = []
               , shapes = M.empty
               , activeViews = []
               , envFps = 1
               }

emptySPM :: ShapePosMap
emptySPM = M.empty


----------------------------------------------------------------------
--- SalsaSomething functions
----------------------------------------------------------------------

-- | Get the shape positions in the active views.
getShapePos :: (SalsaSomething s, Monad s) => SIdent -> s [(VIdent, Position)]
getShapePos i = do
  Context env sm <- getContext
  return $ mapMaybe (look sm) $ activeViews env
  where look sm vi = (vi,) <$> (M.lookup vi sm >>= M.lookup i)

-- | Get the shape positions in all views.
getAllShapePos :: (SalsaSomething s, Monad s) => SIdent -> s [(VIdent, Position)]
getAllShapePos i = do
  Context env sm <- getContext
  return $ mapMaybe (look sm . \(a, _, _) -> a) $ views env
  where look sm vi = (vi,) <$> (M.lookup vi sm >>= M.lookup i)

evalExpr :: (SalsaSomething s, Functor s, Applicative s, Monad s)
            => Expr -> s Integer
evalExpr expr = case expr of
  Plus e0 e1 -> (+) <$> evalExpr e0 <*> evalExpr e1
  Minus e0 e1 -> (-) <$> evalExpr e0 <*> evalExpr e1
  Const n -> return n
  Xproj si -> fst <$> lowestCoordinates si
  Yproj si -> snd <$> lowestCoordinates si
  where lowestCoordinates si = do
          ts <- map snd <$> getAllShapePos si
          case ts of
            [] -> error ("no shape '" ++ si ++ "' defined yet")
            _ -> return $ minimum ts

getEnv :: (SalsaSomething s, Functor s) => s Env
getEnv = contextEnv <$> getContext

getSPM :: (SalsaSomething s, Functor s) => s ShapePosMap
getSPM = contextSPM <$> getContext



----------------------------------------------------------------------
--- SalsaCommand functions
----------------------------------------------------------------------

-- | Join the frames generated by one command with the frames generated by
-- another.  Use the ShapePosMap of the latter.
joinFrames :: SalsaCommand () -> SalsaCommand () -> SalsaCommand ()
joinFrames x y = SalsaCommand f
  where f ctx = (sm1, frms2, ())
          where (_, frms0, ()) = runSC x ctx
                (sm1, frms1, ()) = runSC y ctx
                frms2 = zipWith (++) (trail (length frms1) frms0)
                        (trail (length frms0) frms1)

-- | Run a command in another environment.
withEnv :: (Env -> Env) -> SalsaCommand a -> SalsaCommand a
withEnv f c = SalsaCommand $ \(Context env sm) -> runSC c $ Context (f env) sm

addFrame :: Frame -> SalsaCommand ()
addFrame frm = SalsaCommand $ \(Context _ sm) -> (sm, [frm], ())

putShapePos :: SalsaSomething s => VIdent -> SIdent -> Position -> s ()
putShapePos vi si absP = modifySPM g
  where g spm = M.update (Just . M.insert si absP) vi spm'
          where spm' | vi `M.notMember` spm = M.insert vi M.empty spm
                     | otherwise = spm

updateShapePos :: SIdent -> (Position -> SalsaCommand Position) -> SalsaCommand ()
updateShapePos si pf = do
  ss <- getShapePos si
  forM_ ss $ \(vi, p0) -> do
    p1 <- pf p0
    putShapePos vi si p1

makeShapeGpx :: VIdent -> SIdent -> Position -> SalsaCommand GpxInstr
makeShapeGpx vi si p = do
  ss <- shapes <$> getEnv
  let s = fromMaybe
          (fail ("no shape of the name '" ++ si ++ "' in view '" ++ vi ++ "'"))
          (M.lookup si =<< M.lookup vi ss)
  return $ s p vi

-- | Add all frames.
commitFrames :: ShapePosMap -> SalsaCommand ()
commitFrames old = foldl joinFrames (return ())
                   $ map (createViewFrames . second M.toList)
                   $ M.toList old

-- | Create all frames for a view, using the old positions of the shapes in the
-- view.
createViewFrames :: (VIdent, [(SIdent, Position)]) -> SalsaCommand ()
createViewFrames (vi, ss) = foldl joinFrames (return ())
                            $ map (createShapeFrames vi) ss

-- | Create all frames for a shape in a view, using the old position of the
-- shape.
createShapeFrames :: VIdent -> (SIdent, Position) -> SalsaCommand ()
createShapeFrames vi (si, p0) = do
  spm <- getSPM
  fps <- envFps <$> getEnv
  let p1 = fromMaybe
           (error ("shape '"
                   ++ si ++ "' in view '"
                   ++ vi ++ "' not present in the present"))
           (M.lookup si =<< M.lookup vi spm)
      ps = interpolate fps p0 p1
  forM_ ps (addFrame . (: []) <=< makeShapeGpx vi si)

move :: SIdent -> Pos -> SalsaCommand ()
move i p = updateShapePos i $ \(x0, y0) -> case p of
  Abs x y -> (,) <$> evalExpr x <*> evalExpr y
  Rel xd yd -> do
    xd' <- evalExpr xd
    yd' <- evalExpr yd
    return (x0 + xd', y0 + yd')

command' :: Command -> SalsaCommand ()
command' c = case c of
  Move is p -> mapM_ (`move` p) is
  At c1 i -> withEnv (\env -> env { activeViews = [i] }) $ command' c1
  Par c0 c1 -> command' c0 >> command' c1

command :: Command -> SalsaCommand ()
command c = do
  old <- getSPM
  command' c
  commitFrames old


----------------------------------------------------------------------
--- Salsa commands
----------------------------------------------------------------------

putContext :: Context -> Salsa ()
putContext ctx = Salsa $ const (ctx, [], ())

modifyContext :: (Context -> Context) -> Salsa ()
modifyContext cf = putContext =<< cf <$> getContext

modifyEnv :: (Env -> Env) -> Salsa ()
modifyEnv ef = modifyContext (\(Context env sm) -> Context (ef env) sm)

addView :: VIdent -> Integer -> Integer -> Salsa ()
addView vi w h = modifyEnv $ \env -> env { views = (vi, w, h) : views env }

addGroup :: VIdent -> [VIdent] -> Salsa ()
addGroup gi vis = modifyEnv $ \env -> env { groups = (gi, vis) : groups env }

setActiveViews :: VIdent -> Salsa ()
setActiveViews i = do
  av <- fromMaybe [i] . lookup i . groups <$> getEnv
  modifyEnv $ \env -> env { activeViews = av }

-- | Add a shape the Env and its position to the ShapePosMap.
addShape :: SIdent -> Shape -> Salsa ()
addShape si s = do
  av <- activeViews <$> getEnv
  forM_ av $ \vi ->
    modifyEnv $ \env -> 
      if M.notMember vi $ shapes env
      then env { shapes = M.insert vi M.empty $ shapes env }
      else env

  forM_ av $ \vi -> modifyEnv $ \env -> env {
    shapes = M.update (Just . M.insert si s) vi $ shapes env
    }

liftC :: SalsaCommand a -> Salsa a
liftC c = Salsa f
  where f ctx@(Context env _) = (Context env sm, frms, a)
          where (sm, frms, a) = runSC c ctx

definition :: Definition -> Salsa ()
definition d = case d of
  Viewdef vi w h -> do
    w' <- evalExpr w
    h' <- evalExpr h
    addView vi w' h'
    setActiveViews vi
  View i -> setActiveViews i
  Group gi vis -> addGroup gi vis
  Rectangle si x y w h col -> do
    x' <- evalExpr x
    y' <- evalExpr y
    w' <- evalExpr w
    h' <- evalExpr h
    addShape si $ \(x1, y1) vi -> DrawRect x1 y1 w' h' vi $ getColourName col
    vis <- activeViews <$> getEnv
    forM_ vis $ \vi -> putShapePos vi si (x', y')
  Circle si x y r col -> do
    x' <- evalExpr x
    y' <- evalExpr y
    r' <- evalExpr r
    addShape si $ \(x1, y1) vi -> DrawCirc x1 y1 r' vi $ getColourName col
    vis <- activeViews <$> getEnv
    forM_ vis $ \vi -> putShapePos vi si (x', y')

defCom :: DefCom -> Salsa ()
defCom (Def d) = definition d
defCom (Com c) = liftC $ command c


-- | Generate an animation from a program.
runProg :: Integer -> Program -> Animation
runProg fps prog = (views $ contextEnv ctx1, frms)
  where ctx = Context (emptyEnv { envFps = fps }) emptySPM
        (ctx1, frms, ()) = runS makeFrames ctx
        makeFrames = mapM_ defCom prog
