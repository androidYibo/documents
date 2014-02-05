--
-- Skeleton for Eddy interpreter
-- To be used at the re-exam for Advanced Programming, B1-2013
--
----------------------------
-- Student: Kasper Passov --
-- KU ID:   pvx884        --
----------------------------

{-# OPTIONS_GHC -Wall #-}

module EddyInterp where

import EddyAst

type Line = String
type Buffer = ([Line], [Line])

data Error = Error deriving(Show, Eq)

data EddyState = EddyState { buffers :: [(BufferName, Buffer)],
                             activeBuffer :: BufferName,
                             macros :: [(MacroName, Script)] }
                 deriving(Show, Eq)

newtype EddyCommand a = EC { runEddy :: EddyState -> (a, EddyState) }

instance Monad EddyCommand where
  return a = EC $ \e -> (a, e)
  a >>= f  = EC $ \e ->
              let (j, en) = runEddy a e in 
                runEddy (f j) en 

emptyState :: EddyState
emptyState = EddyState { buffers = [("*scratch*", ([],[]))],
                         activeBuffer = "*scratch*",
                         macros = [] } 

----------------------------------------
-- Functions to manipulate the states --
----------------------------------------

-- returns the state
getState :: EddyCommand EddyState 
getState = EC $ \s -> (s, s) 

-- sets the current state to ns
setState :: EddyState -> EddyCommand () 
setState ns = EC $ const ((), ns)

-------------------------
-- Buffer manipulation --
-------------------------

-- returns the buffer with the name bn
getBuffer :: BufferName -> EddyCommand Buffer
getBuffer bn = do (EddyState nbufs _ _) <- getState
                  case lookup bn nbufs of
                       Nothing -> getBuffer "*scratch*"
                       Just b  -> return b

-- adds a buffer b to the bufferlist 
addBuffer :: (BufferName, Buffer) -> EddyCommand()
addBuffer (bn,b) = do (EddyState bufs _ ma) <- getState 
                      setState(EddyState (bufs++[(bn,b)]) bn ma)

-- removes a buffer unless it is *scratch*
removeBuffer :: EddyCommand()
removeBuffer = do (EddyState b ab ma) <- getState
                  let newb = filter(\(x,_) -> x /= ab) b
                  if ab == "*scratch*"
                    then raiseError 
                    else setState(EddyState newb "*scratch*" ma)

-------------------------
-- Active manipulation --
-------------------------

-- returns the active buffer
getActive :: EddyCommand Buffer
getActive = do (EddyState _ ab _) <- getState
               getBuffer ab

-- sets the active buffer to the buffer with the name bn
changeActive :: BufferName -> EddyCommand() 
changeActive bn = do (EddyState b _ ma) <- getState
                     case lookup bn b of
                       Nothing    -> addBuffer (bn, ([],[])::Buffer)
                       Just _     -> setState (EddyState b bn ma) 

-- update the active buffer to b
updateActive :: Buffer -> EddyCommand()
updateActive b = do (EddyState bufs bn ma)  <- getState
                    newb <- return $ filter (\(x,_) -> x /= bn) bufs ++ [(bn, b)]
                    setState (EddyState newb bn ma) 

------------------------
-- Macro manipulation --
------------------------

-- adds a macro (mn, s) to the current macros
addMacro :: MacroName -> Script -> EddyCommand()
addMacro mn s = do (EddyState b ab ma) <- getState
                   newma <- return $ filter (\(x,_) -> x /= mn) ma ++ [(mn,s)]
                   setState (EddyState b ab newma)

-- runs the macro mn
runMacro :: MacroName -> EddyCommand()
runMacro mn = do (EddyState _ _ ma) <- getState
                 case lookup mn ma of
                   Nothing -> raiseError 
                   Just sc -> script sc

------------------------
--   Error function   --
------------------------

raiseError :: EddyCommand() 
raiseError = do es <- getState 
                setState es

edrepeat :: Int -> Command -> EddyCommand() 
edrepeat 0 _ = do es <- getState -- det her skammer jeg mig over
                  setState es
edrepeat i c = do command c 
                  edrepeat (i-1) c

------------------------
--Script Functionality--
------------------------

script :: Script -> EddyCommand()
script []     = do es <- getState 
                   setState es
script (c:sc) = do command c
                   script sc 

command :: Command -> EddyCommand ()
command c = case c of
              (Buffer bn)    -> changeActive bn 
              (Remove)       -> removeBuffer
              (Macro mn s)   -> addMacro mn s 
              (Call m)       -> runMacro m
              (Repeat i co)  -> edrepeat i co
              _              -> command' c 

command' :: Command -> EddyCommand ()
command' c = do ab <- getActive
                newb <- case c of
                        (Ins l) -> insertLine l ab 
                        (Del)   -> delLine ab
                        (Next)  -> nextLine ab
                        (Prev)  -> prevLine ab 
                        _       -> return ab -- Some error 
                updateActive newb

-----------------------
-- Line manipulation --
-----------------------

insertLine :: Line -> Buffer -> EddyCommand Buffer
insertLine l (lb, la) = return (l:lb, la) 

delLine :: Buffer -> EddyCommand Buffer
delLine (lb, []) = return (lb, []) 
delLine (lb, _:la) = return (lb, la) 

nextLine :: Buffer -> EddyCommand Buffer
nextLine (lb, []) = return (lb, [])
nextLine (lb, lah:la) = return (lah:lb, la) 

prevLine :: Buffer -> EddyCommand Buffer
prevLine ([], la) = return ([], la)
prevLine (lbh:lb, la) = return (lb, lbh:la) 


-----------------------
--        API        --
-----------------------

runeddy :: Script -> EddyCommand Buffer
runeddy sc = do script sc
                getActive

runScript :: Script -> Either Error [(BufferName, String)]
runScript sc = case buflist of 
                  a -> Right a 
                  {- _ -> Left Error -} -- errors are not implementet
      where 
      buflist = map (\(bufname, (lprev, lnext)) -> (bufname, concatMap (++"\n") ((reverse lprev) ++ lnext))) buffs
      (_,(EddyState buffs _ _)) = runEddy ( runeddy sc ) emptyState 

