--
-- Skeleton for Salsa parser
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaParser(parseString, parseFile) where

import SalsaAst
import Text.ParserCombinators.ReadP

type Error = String

-----------------------------
--           API           --
-----------------------------

parseString :: String -> Either Error Program
parseString input =
    case readP_to_S program input of 
	  [(e, [])] -> Right e
	  _			-> Left "parse error"

parseFile :: FilePath -> IO (Either Error Program)
parseFile f = do s <- readFile f
                 return $ parseString s

-----------------------------
--         Grammar         --
-----------------------------

program :: ReadP [DefCom]
program = do skipSpaces 
             dcs <- defComs
             skipSpaces
             return dcs
                   
defComs :: ReadP [DefCom]                   
defComs = (do skipSpaces
              dc <- defCom
              skipSpaces
              dcs <- defComs
              return (dc : dcs))
      <++ (do return [])
                   
defCom :: ReadP DefCom
defCom = token(do c <- command
                  skipSpaces
                  return (Com c))
     <++ token(do def <- definition
                  skipSpaces
                  return (Def def))

definition :: ReadP Definition
definition = viewdef
         <++ rectangle
         <++ circle
         <++ view
         <++ group
         
command :: ReadP Command
command = do ct <- commandTerm
             com <- command' ct
             return com
           
command' :: Command -> ReadP Command           
command' c1 =(do symbol "||"
                 c2 <- command
                 return (Par c1 c2))
          <++(do c <- command'' c1
                 return c)


command'' :: Command -> ReadP Command     
command'' c1 = (do schar '@'
                   ide <- vIdent
                   return (At c1 ide))   
            <++ do return c1
                        
commandTerm :: ReadP Command
commandTerm = token 
              (do schar '{'
                  c <- command
                  schar '}'
                  return c)
           <++(do sIdes <- sIdents
                  symbol "->"
                  p <- pos
                  return (Move sIdes p)) 

     
vIdents :: ReadP [Ident]
vIdents =(do vId <- vIdent
             vIds <- vIdents
             return (vId : vIds))
     <++ (do return []) 

vIdent :: ReadP Ident
vIdent = ident['A'..'Z']


sIdents :: ReadP [Ident]
sIdents =(do sId <- sIdent
             sIds <- sIdents
             return (sId : sIds))
     <++ (do return []) 

sIdent :: ReadP Ident
sIdent = ident['a'..'z']

pos :: ReadP Pos
pos = (do schar '('
          e1 <- expr
          schar ','
          e2 <- expr
          schar ')'
          return (Abs e1 e2))
   <++(do schar '+'
          schar '('
          e1 <- expr
          schar ','
          e2 <- expr
          schar ')'
          return (Rel e1 e2))
          

expr :: ReadP Expr
expr = do e1 <- prim
          e <- expr' e1
          return e

expr' :: Expr -> ReadP Expr          
expr' e1 = (do schar '+'
               e2 <- expr
               return (Plus e1 e2))
        <++(do schar '-'
               e2 <- expr
               return (Minus e1 e2))
        <++ return e1
          
prim :: ReadP Expr
prim = (do int <- integer
           return (Const int))
    <++(do ide <- sIdent 
           schar '.'
           ideXY <- (prim' ide)
           return ideXY)
    <++(do schar '('
           e <- expr
           schar ')'
           return e)

colour :: ReadP Colour
colour = (symbol "blue"  >> return Blue)
      <++(symbol "plum"  >> return Plum) -- i don't think the do notation made this prettier
      <++(symbol "red"   >> return Red)
      <++(symbol "green" >> return Green)
      <++(symbol "orange">> return Orange)
     

-----------------------------
-- Internal Implementation --
-----------------------------

ident :: [Char] -> ReadP Ident 
ident fl = token(do first <- satisfy(`elem` fl) -- checks the first char is in the allowed list fl
                    iv <- idents     
                    skipSpaces
                    if checkIdents ([first] ++ iv)
                       then pfail
                       else return ([first] ++ iv))
                 where idents = munch(`elem` allowed) -- parses the rest of the ident
                       checkIdents s = s `elem` notallowed
                       allowed = '_':['a'..'z']++['A'..'Z']++['0'..'9']
                       notallowed = ["viewdef", "rectangle", "circle", "broup", "view",
                                     "blue", "plum", "red", "green", "orange", "x", "y"]

viewdef :: ReadP Definition
viewdef = do skipSpaces
             symbol "viewdef"
             vIde <- vIdent
             e1 <- expr
             e2 <- expr
             return(Viewdef vIde e1 e2)

rectangle :: ReadP Definition
rectangle = do symbol "rectangle"
               sIde <- sIdent
               e1 <- expr
               e2 <- expr
               e3 <- expr
               e4 <- expr
               c  <- colour
               return(Rectangle sIde e1 e2 e3 e4 c)
               
circle :: ReadP Definition
circle = do symbol "circle"
            sIde <- sIdent
            e1 <- expr
            e2 <- expr
            e3 <- expr
            c  <- colour
            return(Circle sIde e1 e2 e3 c)
            
view :: ReadP Definition
view = do symbol "view"
          skipSpaces
          vIde <- vIdent
          return(View vIde)
          
group :: ReadP Definition
group = do symbol "group"
           vIde <- vIdent
           schar '['
           vIdes <- vIdents
           schar ']'
           return(Group vIde vIdes)
           
            
prim' :: Ident -> ReadP Expr
prim' ide = (do schar 'x'
                return (Xproj ide))
         <++(do schar 'y'
                return (Yproj ide))

integer :: ReadP Integer
integer = token (do stringInt <- integers
                    skipSpaces
                    return (read stringInt::Integer))
                 where integers = munch1(`elem` ['0'..'9'])


-- Lexical combinators: taken from SimpleParse --------------------------

token           :: ReadP a -> ReadP a
token p          = skipSpaces >> p 

symbol			:: String -> ReadP String
symbol           = token . string

schar			:: Char -> ReadP Char
schar            = token . char
