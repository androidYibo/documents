{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{---------------------------------------------------------------------
Salsa parser.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module SalsaParser (
  parseString,
  parseFile,
  Error
  ) where


import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator
import Control.Monad
import Control.Applicative (Applicative, (<$>), (<*>), (<*))
import Data.Char

import Misc
import SalsaAst


type Error = ParseError


program :: Parser Program
program = whiteSpaces >> lexeme (many1 defCom) <* eof
          <?> "Program"


defCom :: Parser DefCom
defCom = lexeme defCom' <?> "DefCom"

defCom' :: Parser DefCom
defCom' = Def <$> definition
          <|> Com <$> command


definition :: Parser Definition
definition = lexeme definition' <?> "Definition"

definition' :: Parser Definition
definition' = choice [viewdef, view, rectangle, circle, group]
  where viewdef = lexeme (try (string "viewdef") >> whiteSpace)
                  >> Viewdef <$> vIdent <*> expr <*> expr
        rectangle = lexeme (try (string "rectangle") >> whiteSpace)
                    >> Rectangle <$> sIdent
                    <*> expr <*> expr <*> expr <*> expr <*> colour
        circle = lexeme (try (string "circle") >> whiteSpace)
                 >> Circle <$> sIdent
                 <*> expr <*> expr <*> expr <*> colour
        view = lexeme (try (string "view") >> whiteSpace)
               >> View <$> vIdent
        group = lexeme (try (string "group") >> whiteSpace)
                >> Group <$> vIdent <*> inBraces (many1 vIdent)


command :: Parser Command
command = lexeme command' <?> "Command"

command' :: Parser Command
command' = par
  where par = chainl1 at (symbol "||" >> return Par)
        at = chainpre1 (move <|> inBrackets command)
             (symbol "@" >> fmap (flip At) vIdent)
        move = do
          is <- many1 sIdent
          symbol "->"
          p <- pos
          return $ Move is p


vIdent :: Parser Ident
vIdent = lexeme vIdent' <?> "VIdent"

vIdent' :: Parser Ident
vIdent' = satisfy isUpper <:> baseIdent


sIdent :: Parser Ident
sIdent = lexeme (try sIdent') <?> "SIdent"

sIdent' :: Parser Ident
sIdent' = do
  i <- satisfy isLower <:> baseIdent
  guard (i `notElem` reservedWords)
  return i


baseIdent :: Parser Ident
baseIdent = many (alphaNum <|> char '_')


pos :: Parser Pos
pos = lexeme pos' <?> "Pos"

pos' :: Parser Pos
pos' = absp <|> relp
  where absp = inParens $ Abs <$> expr <*> (symbol "," >> expr)
        relp = symbol "+" >> fmap (\(Abs a b) -> Rel a b) absp
        

expr :: Parser Expr
expr = lexeme expr' <?> "Expr"

expr' :: Parser Expr
expr' = chainpre1 prim $ choice
        [ symbol "+" >> fmap (flip Plus) prim
        , symbol "-" >> fmap (flip Minus) prim
        ]

prim :: Parser Expr
prim = lexeme prim' <?> "Prim"

prim' :: Parser Expr
prim' = integer <|> dot <|> inParens expr
  where integer = Const . read <$> many1 digit
        dot = do
          i <- sIdent <* lexeme (char '.')
          choice [ char 'x' >> return (Xproj i)
                 , char 'y' >> return (Yproj i)
                 ]


colour :: Parser Colour
colour = lexeme colour' <?> "Colour"

colour' :: Parser Colour
colour' = choice $ map toRule colours
  where toRule (name, constr) =
          lexeme (string name >> (void whiteSpace <|> eof)) >> return constr

  
-- | Parse a Salsa program.
parseString :: String -> Either Error Program
parseString = parse program "<stdin>"

-- | Parse a Salsa program from file.
parseFile :: FilePath -> IO (Either Error Program)
parseFile = parseFromFile program
