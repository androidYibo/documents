{-
  Random Testing using QuickCheck for SalsaParser
-}
module SalsaParserTest where

import qualified Test.QuickCheck as QC
import qualified Test.HUnit as HU

import qualified SalsaParser as SP
import SalsaAst

{-
  Generating random ASTs
  
  We generate the ASTs and its source representation at the same time. For each
  possible rule in the grammar, we generate a random concrete instance of such
  rule, which includes possible random sub-components (non-terminals) used in
  the rule), consisting of an AST representation and a string representation 
  of that rule instance. These two representations are then compounded into
  the entire source and complete AST of the program, respectively.
  
-}

--------------
-- Whitespaces
-- NOT REALLY USED FOR THIS TESTING
--------------
whitespaces :: String
whitespaces = " \t\n"
newtype Whitespace = WS Char
instance QC.Arbitrary Whitespace where
  arbitrary = do c <- QC.elements whitespaces
                 return $ WS c

someBlanks :: QC.Gen String
someBlanks = do bs <- QC.arbitrary -- More spaces: bs <- QC.listOf QC.arbitrary
                return $ map unpack bs
           where unpack (WS c) = c

-- someBlanks1 :: QC.Gen String
-- someBlanks1 = do (WS b) <- QC.arbitrary
                 -- bs <- someBlanks
                 -- return (b:bs)
someBlanks1 :: QC.Gen String
someBlanks1 = return " "

-----------
-- Integers
-----------
newtype ConstInt = ConstInt Integer deriving (Eq, Show)
instance QC.Arbitrary ConstInt where
  arbitrary = do 
    (QC.NonNegative n) <- QC.arbitrary
    return $ ConstInt n

--------- 
-- Idents
---------
videntStart :: String
videntStart = ['A'..'Z']
sidentStart :: String
sidentStart = ['a'..'z']
nameChars :: String
nameChars = sidentStart ++ videntStart ++ ['0'..'9'] ++ "_"

-- Max length of generated identifiers
identSize :: Int
identSize = 10

-- Max number of idents in lists of identifiers
idListMax :: Int
idListMax = 3
 
newtype TestVIdent = TestVIdent (String, String) deriving (Eq, Show)
instance QC.Arbitrary TestVIdent where
  arbitrary = do s <- getIdent videntStart
                 return $ TestVIdent (s, s)
    
newtype TestSIdent = TestSIdent (String, String) deriving (Eq, Show)
instance QC.Arbitrary TestSIdent where
  arbitrary = do s <- getIdent sidentStart
                 return $ TestSIdent (s, s)

getIdent :: String -> QC.Gen String
getIdent firstChar = do 
  c <- QC.elements firstChar
  cs <- QC.vectorOf identSize $ QC.elements nameChars
  return (c:cs)
  
getVIdents :: QC.Gen [TestVIdent]
getVIdents = QC.vectorOf idListMax QC.arbitrary

getSIdents :: QC.Gen [TestSIdent]
getSIdents = QC.vectorOf idListMax QC.arbitrary

---------
-- Colour
---------
colors :: QC.Gen (String, Colour)
colors = QC.elements
         [("blue", Blue), ("plum", Plum),
          ("red", Red), ("green", Green),
          ("orange", Orange)]
    
--------------
-- Expressions
--------------
projs :: [(String, String -> Expr)]
projs = [("x", Xproj), ("y", Yproj)]
newtype TestPrim = TestPrim (String, Expr) deriving (Eq, Show)
instance QC.Arbitrary TestPrim where
  arbitrary = QC.frequency [(50, anInteger), (20, aProj), (5, aPExpr)]
    where
      anInteger = do 
        (ConstInt n) <- QC.arbitrary
        return $ TestPrim (show n, Const n)
      aProj = do
        (TestSIdent (sid, _)) <- QC.arbitrary
        (dim, proj) <- QC.elements projs
        let s = sid ++ "." ++ dim
        return $ TestPrim (s, proj sid)
      aPExpr = do
        (TestExpr (s, e)) <- QC.arbitrary
        let s' = "(" ++ s ++ ")"
        return $ TestPrim (s', e)

arithOps :: [(String, Expr -> Expr -> Expr)]
arithOps = [(" + ", Plus), (" - ", Minus)]
newtype TestExpr = TestExpr (String, Expr) deriving (Eq, Show)
instance QC.Arbitrary TestExpr where
  arbitrary = QC.frequency [(10, aPrim), (1, anArith)]
    where
      aPrim = do 
        (TestPrim p) <- QC.arbitrary
        return $ TestExpr p
      anArith = do
        (TestExpr (s1, e)) <- QC.arbitrary
        (TestPrim (s2, p)) <- QC.arbitrary
        (sign, op) <- QC.elements arithOps
        let s = s1 ++ sign ++ s2
        return $ TestExpr (s, op e p)

------
-- Pos
------
posTypes :: [(String, Expr -> Expr -> Pos)]
posTypes = [("+", Rel), ("", Abs)]
newtype TestPos = TestPos (String, Pos) deriving (Eq, Show)
instance QC.Arbitrary TestPos where
  arbitrary = do
    (TestExpr (s1, e1)) <- QC.arbitrary
    (TestExpr (s2, e2)) <- QC.arbitrary
    (sign, con) <- QC.elements posTypes
    let s = sign ++ "(" ++ s1 ++ ", " ++ s2 ++ ")"
    return $ TestPos (s, con e1 e2)

--------------
-- Definitions
--------------
newtype TestDef = TestDef (String, Definition) deriving (Eq, Show)
instance QC.Arbitrary TestDef where
  arbitrary = QC.frequency [(1, aViewdef)
                           ,(1, aRect)
                           ,(1, aCir)
                           ,(1, aView)
                           ,(1, aGroup)]
    where
      aViewdef = do
        (TestVIdent (vid,_)) <- QC.arbitrary
        (TestExpr (s1, e1)) <- QC.arbitrary
        (TestExpr (s2, e2)) <- QC.arbitrary
        let s = unwords ["viewdef", vid, s1, s2]
        return $ TestDef (s, Viewdef vid e1 e2)
      fig = do
        (TestSIdent (sid,_)) <- QC.arbitrary
        (TestExpr (s1, e1)) <- QC.arbitrary
        (TestExpr (s2, e2)) <- QC.arbitrary
        (TestExpr (s3, e3)) <- QC.arbitrary
        return (sid, s1, e1, s2, e2, s3, e3)
      aRect = do
        (sid, s1, e1, s2, e2, s3, e3) <- fig
        (TestExpr (s4, e4)) <- QC.arbitrary
        (s5, c) <- colors
        let s = unwords ["rectangle", sid, s1, s2, s3, s4, s5]
        return $ TestDef (s, Rectangle sid e1 e2 e3 e4 c)
      aCir = do
        (sid, s1, e1, s2, e2, s3, e3) <- fig
        (s5, c) <- colors
        let s = unwords ["circle", sid, s1, s2, s3, s5]
        return $ TestDef (s, Circle sid e1 e2 e3 c)
      aView = do
        (TestVIdent (vid,_)) <- QC.arbitrary
        let s = unwords ["view", vid]
        return $ TestDef (s, View vid)
      aGroup = do
        (TestVIdent (vid,_)) <- QC.arbitrary
        vs <- getVIdents
        let vids = map unpack vs
        let ss = ["group", vid, "["] ++ vids ++ ["]"]
        let s = unwords ss
        return $ TestDef (s, Group vid vids)
      unpack (TestVIdent (vid, _)) = vid        

-----------
-- Commands
-----------        
newtype TestCom = TestCom (String, Command) deriving (Eq, Show)
instance QC.Arbitrary TestCom where
  arbitrary = QC.frequency [(10, aCommandA), (1, aParCommand)]
    where
      aCommandA = do
        (TestComA c) <- QC.arbitrary
        return $ TestCom c
      aParCommand = do
        (TestCom (s1, c1)) <- QC.arbitrary
        (TestComA (s2, c2)) <- QC.arbitrary
        let ss = unwords [s1, "||", s2]
        return $ TestCom (ss, Par c1 c2)
    
newtype TestComA = TestComA (String, Command) deriving (Eq, Show)
instance QC.Arbitrary TestComA where
  arbitrary = QC.frequency [(5, aCommandF), (1, anAtCommand)]
    where
      aCommandF = do
        (TestComF c) <- QC.arbitrary
        return $ TestComA c
      anAtCommand = do
        (TestComA (s1, c1)) <- QC.arbitrary
        (TestVIdent (vid, _)) <- QC.arbitrary
        let ss = unwords [s1, "@", vid]
        return $ TestComA (ss, At c1 vid)
  
newtype TestComF = TestComF (String, Command) deriving (Eq, Show)
instance QC.Arbitrary TestComF where
  arbitrary = QC.frequency [(10, aMove), (1, aPCommand)]
    where
      aMove = do
        sid_list <- getSIdents
        (TestPos (s_pos, pos)) <- QC.arbitrary
        let sids = map unpack sid_list
        let ss = sids ++ ["->", s_pos]
        let s = unwords ss
        return $ TestComF (s, Move sids pos)
      unpack (TestSIdent (sid, _)) = sid
      aPCommand = do
        (TestCom (s, c)) <- QC.arbitrary
        let ss = "{" ++ s ++ "}"
        return $ TestComF (ss, c)
             
----------
-- DefComs
----------
newtype TestDefCom = TestDefCom (String, DefCom) deriving (Eq, Show)
instance QC.Arbitrary TestDefCom where
  arbitrary = QC.frequency [(2, aDefinition), (1, aCommand)]
    where
      aDefinition = do
        (TestDef (s, d)) <- QC.arbitrary
        return $ TestDefCom (s, Def d)
      aCommand = do
        (TestCom (s, c)) <- QC.arbitrary
        return $ TestDefCom (s, Com c)
    
-----------
-- Programs
-----------
newtype TestProgram = TestProgram (String, Program)
  deriving (Eq, Show)

instance QC.Arbitrary TestProgram where
  arbitrary = do
    defComs <- QC.listOf1 QC.arbitrary
    let ts = map unpack defComs
    let ss = map fst ts
    let ds = map snd ts
    ptxt <- makeSource ss
    return $ TestProgram (ptxt, ds)
    
    where 
      makeSource :: [String] -> QC.Gen String
      makeSource ss = return $ unwords ss
      -- makeSource [] = return ""
      -- makeSource (s:ss) = do
        -- bs <- someBlanks1
        -- rest <- makeSource ss
        -- return (s ++ bs ++ rest)
      
      unpack (TestDefCom sd) = sd

-------------
-- Properties
-------------

-- Property 1
prop_pDeterminism :: TestProgram -> Bool
prop_pDeterminism (TestProgram (s, ast)) =
  SP.parseString s == Right ast
  
-- Reject programs that type as ASTs, but are illegal according to the grammar
tEmptyLists :: [(String, String)]
tEmptyLists = [("No defcoms", "")
              ,("No SIdent in move", " -> (1,1)")
              ,("No VIdent in group", "group A [ ]")]
tWrongSIdent :: [(String, String)]
tWrongSIdent = [("Upper case", "rectangle Ax 1 1 1 1 blue")
               ,("Digit"     , "rectangle 9x 1 1 1 1 blue")
               ,("Underscore", "rectangle _x 1 1 1 1 blue")
               ,("Reserved 1", "rectangle view 1 1 1 1 blue")
               ,("Reserved 2", "rectangle plum 1 1 1 1 blue")]
tWrongVIdent :: [(String, String)]
tWrongVIdent = [("Lower case", "viewdef aB 1 1")
               ,("Digit"     , "viewdef 9B 1 1")
               ,("Underscore", "viewdef _B 1 1")]

pParseError :: Either SP.Error Program -> Bool
pParseError (Left _) = True
pParseError _        = False

tParseError :: (String, String) -> HU.Test
tParseError (tName, input) = 
  HU.TestCase $ HU.assertBool tName $ pParseError $ SP.parseString input

testsHU :: HU.Test
testsHU = HU.TestList 
  [HU.TestLabel "Non-empty lists" $ HU.TestList (map tParseError tEmptyLists)
  ,HU.TestLabel "Wrong SIdent"    $ HU.TestList (map tParseError tWrongSIdent)
  ,HU.TestLabel "Wrong VIdent"    $ HU.TestList (map tParseError tWrongVIdent)]

-- Done with test cases

runTests :: IO HU.Counts
runTests = do
  QC.quickCheck prop_pDeterminism
  HU.runTestTT testsHU
  
runManyTests :: IO ()
runManyTests = QC.quickCheckWith QC.stdArgs {QC.maxSuccess = 10000} 
               prop_pDeterminism
  