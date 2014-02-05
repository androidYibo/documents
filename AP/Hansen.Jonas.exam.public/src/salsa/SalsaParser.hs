{-
  Parser for Salsa
  
  Based on:
  Skeleton for Salsa parser
  To be used at the exam for Advanced Programming, B1-2013
  
  Student:  Jonas Stig Kaempf Hansen
  KU-id:    

-}

module SalsaParser
  ( Error
  , parseString
  , parseFile
  ) where

import Data.Char
import Control.Monad(liftM4)
import qualified Data.Map as Map
import SimpleParse

import SalsaAst

{--------------
  Parsing rules for grammar
--------------}

program :: Parser Program
program = defcoms

defcoms :: Parser [DefCom]
defcoms = many1 defcom

defcom :: Parser DefCom
defcom = (do d <- definition
             return $ Def d)
       <|> (do c <- command
               return $ Com c)

definition :: Parser Definition
definition = viewdef <|> rectangle <|> circle <|> view <|> group
  where
    viewdef = 
       do symbol "viewdef"
          vid <- vident
          e1 <- expr
          e2 <- expr
          return $ Viewdef vid e1 e2
    fig = liftM4 (,,,) sident expr expr expr
    rectangle = 
       do symbol "rectangle"
          (sid,e1,e2,e3) <- fig
          e4 <- expr
          c <- colour
          return $ Rectangle sid e1 e2 e3 e4 c
    circle = 
       do symbol "circle"
          (sid,e1,e2,e3) <- fig
          c <- colour
          return $ Circle sid e1 e2 e3 c
    view = 
       do symbol "view"
          vid <- vident
          return $ View vid
    group = 
       do symbol "group"
          vid <- vident
          symbol "["
          vids <- vidents
          symbol "]"
          return $ Group vid vids

command :: Parser Command
command = commandA `chainl1` connectop
        where connectop = do symbol "||"
                             return Par
          
commandA :: Parser Command
commandA = do f <- commandF
              cAopt f
          
cAopt :: Command -> Parser Command
cAopt c = (do symbol "@"
              vid <- vident
              cAopt $ At c vid)
        <|> return c
          
commandF :: Parser Command
commandF = (do sids <- sidents
               symbol "->"
               p <- pos
               return $ Move sids p) 
        <|> (do symbol "{"
                c <- command
                symbol "}"
                return c)

vidents :: Parser [Ident]
vidents = many1 vident

sidents :: Parser [Ident]
sidents = many1 sident

pos :: Parser Pos
pos = do s <- option (symbol "+")
         symbol "("
         e1 <- expr
         symbol ","
         e2 <- expr
         symbol ")"
         case s of
           Nothing -> return $ Abs e1 e2
           Just _  -> return $ Rel e1 e2

expr :: Parser Expr
expr = prim `chainl1` (plus <|> minus)
     where plus = do symbol "+"
                     return Plus
           minus = do symbol "-"
                      return Minus

prim :: Parser Expr
prim = constn <|> proj <|> parExpr
     where constn = do n <- integer
                       return $ Const n
           proj = do sid <- sident
                     symbol "."
                     dim sid
           parExpr = do symbol "("
                        e <- expr
                        symbol ")"
                        return e

dim :: Ident -> Parser Expr
dim sid = opt "x" Xproj <|> opt "y" Yproj
         where opt s t = do symbol s
                            return $ t sid

colour :: Parser Colour
colour = token (do name <- many1 (satisfy isLetter)
                   case Map.lookup name colors of
                     Just c -> return c
                     Nothing -> reject)

{--------------
  Reserved symbols
--------------}

keywords :: [String]
keywords = ["viewdef", "rectangle", "circle", "group", "view"]

colors :: Map.Map String Colour
colors = Map.fromList [("blue", Blue)
                      ,("plum", Plum)
                      ,("red", Red)
                      ,("green", Green)
                      ,("orange", Orange)
                      ]

reserved :: [String]
reserved = keywords ++ Map.keys colors

{--------------
  Parsers for identifiers and numbers
  [Losely based on my hand-in for assignment 1]
--------------}

vident :: Parser Ident
vident = ident isUpper

sident :: Parser Ident
sident = ident isLower

-- Get identifier begining with symbol satisfying predicate p
ident :: (Char -> Bool) -> Parser Ident
ident p = token (do first <- satisfy p
                    rest <- many (letter <|> num <|> undsc)
                    let name = first:rest in
                      if name `notElem` reserved then return name
                      else reject)
          where letter = satisfy isLetter
                num = satisfy isDigit
                undsc = char '_'

integer :: Parser Integer
integer = token (do n <- digits
                    return $ read n)

digits :: Parser String
digits = many1 (satisfy isDigit)

{--------------
  Module API
--------------}

-- Type for parse errors
data Error = Error deriving (Show, Eq)

parseString :: String -> Either Error Program
parseString input = case par_result of
                      (p, ""):_ -> Right p
                      _ -> Left Error
                  where par_result = parse (do prog <- program
                                               token eof
                                               return prog) input
                                               
parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename
