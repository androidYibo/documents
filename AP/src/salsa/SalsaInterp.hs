--
-- Skeleton for Salsa interpreter
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp (Position, interpolate, runProg) where

import SalsaAst
import Gpx

--
-- The function interpolate
--

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
interpolate 0 _ _ = []
interpolate n (startX, startY) (endX, endY) =
    (newX,newY) : (interpolate (n-1) (newX, newY) (endX, endY))
    where newX = startX + ((endX - startX) `div` n) 
          newY = startY + ((endY - startY) `div` n)
    
-- nextframe = (pstart + delta/n)
-- 4 (-10,0)        (40,20)
--    -10   + 50/4 =  3
--        0 + 20/4 =     5

-- 3 (3,5)          (40,20)
--    3   + 37/3   = 15
--      5 + 15/3   =    10

-- 2 (15,10)        (40,20)
--    15    + 25/2 = 28
--       10 + 10/2 =    15

-- 1 (28,15)        (40,20)
--    28    + 12/1 = 40
--       15 + 5/1  =    20




--
-- Define the types Context and SalsaCommand
--

data MutableContex

type Mutable = [Definition]              
              
data Context = Context{ views  :: [Ident],              -- list of view id's
                        groups :: [(Ident, [Ident])],   -- list of group id's
                        shapes :: [Definition],         -- list of shapes
                        active :: [Ident],              -- the active view
                        frrate :: Integer,              -- the framerate
                        -- mutable
                        shapesmut :: Mutable  }
                deriving (Eq, Show)
             
             
newtype SalsaCommand a = SalsaCommand {runSC :: Context -> (a, Mutable) }

instance Monad SalsaCommand where
-- a -> m a
    return a = SalsaCommand $ \c -> (a, shapesmut c)
-- m a -> (a -> m b) -> m b
    a >>= f = SalsaCommand $ \c ->
                let (j,k) = (runSC a) c in
                    runSC (f j) $ c { shapesmut = k }  
-- m a -> m b -> m b
    a >> b = SalsaCommand $ \c ->
                let (_,k) = (runSC a) c in
                    runSC b $ c { shapesmut = k }
                    
                    
-- functions for manipulating the context
getContext :: SalsaCommand Context
getContext = SalsaCommand $ \c -> (c, shapesmut c)

setContext :: Mutable -> SalsaCommand ()
setContext muta = SalsaCommand $ \_ -> ((),muta)

local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a 
local f m = SalsaCommand $ \c -> runSC m (f c)
                                         
--
-- Define the function command
--

command :: Command -> SalsaCommand ()

command (Move idents pos) = 
    do c <- getContext
       setContext (updateMove pos idents c)
command (At cmd idents) = do con <- getContext 
                             local (\c -> con { active = [idents] }) (command cmd)
command (Par cmd1 cmd2) = command cmd1 >> command cmd2

-- helper functions

updateMove :: Pos -> [Ident] -> Context -> Mutable
updateMove pos ids cupm = let [ret] = map (\idum -> fun1 idum cupm pos ) ids in
                               ret

fun1 :: Ident -> Context -> Pos -> Mutable
fun1 idf1 c pos = map (\shape -> containsId shape idf1 pos) (shapesmut c)
      
containsId :: Definition -> Ident -> Pos -> Definition
containsId shape idci pos=
        case shape of
            (Rectangle shid e1 e2 e3 e4 c) -> if shid == idci
                                            then let (ne1,ne2) = changePos pos e1 e2 in
                                                (Rectangle shid ne1 ne2 e3 e4 c)
                                            else shape
            (Circle shid e1 e2 e3 c)       -> if shid == idci
                                                then let (ne1,ne2) = changePos pos e1 e2 in
                                                     (Circle shid ne1 ne2 e3 c)
                                                else shape
            _   -> shape                       
changePos :: Pos -> Expr -> Expr -> (Expr, Expr)                                                
changePos pos e1 e2 = 
        case pos of
            Abs ne1 ne2 -> (ne1, ne2)
            Rel ne1 ne2 -> ((Plus e1 ne1), (Plus e2 ne2)) --(e1 + ne1, e2 + ne2)
       
--
-- Define the type Salsa
--

--data Salsa a = 


--
-- Define the functions liftC, definition, and defCom
--

--liftC :: SalsaCommand a -> Salsa a

--definition :: Definition -> Salsa ()

--defCom :: DefCom -> Salsa ()


--
-- Define the function runProg
--

--runProg :: Integer -> Program -> Animation
