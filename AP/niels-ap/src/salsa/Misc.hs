{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}
{---------------------------------------------------------------------
Miscellaneous values and functions useful in the Salsa universe.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module Misc where

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative hiding (Const, (<|>), many)
import Control.Monad
import Test.QuickCheck.Gen
import System.Random (StdGen)
import Data.Maybe (fromMaybe)

import SalsaAst
import Gpx


----------------------------------------------------------------------
--- Misc. core
----------------------------------------------------------------------

type Position = (Integer, Integer)

reservedWords :: [String]
reservedWords = ["viewdef", "rectangle", "circle", "group", "view"]
                ++ map fst colours

colours :: [(String, Colour)]
colours = [ ("blue", Blue)
          , ("plum", Plum)
          , ("red", Red)
          , ("green", Green)
          , ("orange", Orange)
          ]


----------------------------------------------------------------------
--- Misc. extensions
----------------------------------------------------------------------

(<:>) :: Applicative f => f a -> f [a] -> f [a]
x <:> xs = (:) <$> x <*> xs

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
xs <++> ys = (++) <$> xs <*> ys

trail :: Int -> [[a]] -> [[a]]
trail n xs = xs ++ replicate (max 0 (n - length xs)) []

getColourName :: Colour -> String
getColourName col = fromMaybe (error "colour does not exist")
                    $ lookup col $ map (\(a, b) -> (b, a)) colours

getColour :: String -> Colour
getColour col = fromMaybe (error "colour does not exist")
                $ lookup col colours

nFrames :: Animation -> Int
nFrames (_, frms) = length frms

tryEvalExpr :: Expr -> Maybe Integer
tryEvalExpr e = case e of
  Plus a b -> (+) <$> tryEvalExpr a <*> tryEvalExpr b
  Minus a b -> (-) <$> tryEvalExpr a <*> tryEvalExpr b
  Const a -> return a
  Xproj _ -> Nothing
  Yproj _ -> Nothing


----------------------------------------------------------------------
--- QuickCheck extensions
----------------------------------------------------------------------

-- | Generates a value.
genVal :: StdGen -> Gen a -> a
genVal r g = unGen g r 0


----------------------------------------------------------------------
--- Parsec extensions
----------------------------------------------------------------------

whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = void (space <|> char '\n')

whiteSpaces :: Stream s m Char => ParsecT s u m ()
whiteSpaces = void (many whiteSpace)

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = p <* whiteSpaces

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = lexeme . try . string

inParens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
inParens = lexeme . between (lexeme $ char '(') (char ')')

inBrackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
inBrackets = lexeme . between (lexeme $ char '{') (char '}')

inBraces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
inBraces = lexeme . between (lexeme $ char '[') (char ']')

-- | Parse a chain of infix expressions (a ::= a op b).
chainpre1 :: Stream s m Char =>
             ParsecT s u m a ->
             ParsecT s u m (a -> a) ->
             ParsecT s u m a
chainpre1 p0 op = p0 >>= chain'
  where chain' t = (try (($ t) <$> op) >>= chain')
                   <|> return t
