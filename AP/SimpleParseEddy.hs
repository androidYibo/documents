--
-- Skeleton for Eddy parser
-- To be used at the re-exam for Advanced Programming, B1-2013
--
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module EddyParser 
  {- ( Error -}
  {- , EddyParser.parse -}
  {- , parseFile -}
  {- ) where -}
  where

import SimpleParse
import Data.Char
import EddyAst

data Error = Error deriving (Show, Eq) 

-----------------------------
--           API           --
-----------------------------

{- parseFile :: FilePath -> IO (Either Error Script) -}
{- parseFile f = do s <- readFile f -}
                 {- return $ EddyParser.parse s  -}

parse :: String -> Either Error Script
parse input = case output of
    (p, ""):_ -> Right p
    _         -> Left Error
  where output = SimpleParse.parse (do 
                  sc <- script
                  token eof
                  return sc) input
                
{- parseids :: String -> Either Error [String] -}
parseids input = case output of
    (p, ""):_ -> Right p
    _         -> Left Error
  where output = SimpleParse.parse (do 
                  sc <- idents
                  token eof
                  return sc) input

parseid input = case output of
    (p, ""):_ -> Right p
    _         -> Left Error
  where output = SimpleParse.parse (do 
                  sc <- ident
                  token eof
                  return sc) input
-----------------------------
-- Internal Implementation --
-----------------------------

script :: Parser Script
script = commands

commands :: Parser [Command]
commands = do c <- command
              cs <- commands
              return (c : cs)
          <|> return []

command :: Parser Command
command = do ct <- commandTerm 
             rest ct
          where 
             rest ct = edrepeat ct 
                   <|> return ct

edrepeat :: Command -> Parser Command
edrepeat ct = do symbol "*"
                 i <- integer
                 edrepeat $ Repeat i ct
             <|> return ct

commandTerm :: Parser Command
commandTerm = del    <|> next  <|> prev  <|> remove <|> 
              buffer <|> macro <|> call  <|> insert 
            where
              insert = do symbol "i"
                          i <- integer
                          symbol "|"
                          cn <- char_n i
                          return (Ins cn)                
              del    = do symbol "del"
                          return Del
              next   = do symbol "next"
                          return Next 
              prev   = do symbol "prev"
                          return Prev
              remove = do symbol "remove"
                          return Remove 
              buffer = do symbol "buffer"
                          s <- ident
                          return (Buffer s)
              macro  = do symbol "macro"
                          st <- ident
                          symbol "{"
                          sc <- script 
                          symbol "}"
                          return (Macro st sc)
              call   = do st <- ident
                          return (Call st)

integer :: Parser Int
integer = token (do n <- many1 (satisfy isDigit)
                    return $ read n)

char_n :: Int -> Parser [Char]
char_n i = token (do c <- many1 (satisfy isLetter)
                     if length(c) == i
                          then return c
                          else reject)

idents :: Parser [String]
idents = do ids <- many ident
            return ids

{- identwiths = do many1 space -}
                {- id <- ident -}
                {- return id -}

ident :: Parser [Char] 
ident = token(do cs <- many1 (digits <|> letters <|> chars) 
                 space
                 if checkIdents(cs)
                    then return cs
                    else reject)
            where digits  = satisfy isDigit
                  letters = satisfy isLetter
                  chars   = satisfy (`elem` ['_', '.', '/', '*', '?'])
                  checkIdents s = s `notElem` reservedKeywords 

reservedKeywords :: [String]
reservedKeywords = ["i", "del", "next", "prev", "buffer", "remove", "macro"] 

