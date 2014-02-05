--
-- Skeleton for Eddy parser
-- To be used at the re-exam for Advanced Programming, B1-2013
-- 

----------------------------
-- Student: Kasper Passov --
-- KU ID:   pvx884        --
----------------------------

{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module EddyParser 
  ( Error
  , EddyParser.parse
  , parseFile
  , contains
  ) where

import Text.ParserCombinators.ReadP 
import Data.Char
import EddyAst

data Error = Error deriving (Show, Eq) 

-----------------------------
--           API           --
-----------------------------

parseFile :: FilePath -> IO (Either Error Script)
parseFile f = do s <- readFile f
                 return $ EddyParser.parse s 

parse :: String -> Either Error Script
parse input = case output of
    (p, ""):_ -> Right p
    _         -> Left Error
  where output = readP_to_S (do 
                  sc <- script
                  skipSpaces 
                  eof
                  return sc) input
                
-----------------------------
-- Internal Implementation --
-----------------------------

script :: ReadP Script
script = commands

commands :: ReadP [Command]
commands = many command 

command :: ReadP Command
command = do ct <- commandTerm 
             rest ct
          where 
             rest ct = edrepeat ct 
                   +++ return ct

edrepeat :: Command -> ReadP Command
edrepeat ct = do skstring "*"
                 i <- integer
                 edrepeat $ Repeat i ct -- edrepeat is run agein if we have nested repeats
             +++ return ct

commandTerm :: ReadP Command
commandTerm = insert +++ del    +++ next  +++ prev +++ 
              buffer +++ remove +++ macro +++ call 
            where
              insert = do skstring "i"
                          i <- integer
                          skstring "|"
                          cn <- charN i
                          return (Ins cn)                
              del    = do skstring "del"
                          return Del
              next   = do skstring "next"
                          return Next 
              prev   = do skstring "prev"
                          return Prev
              buffer = do skstring "buffer"
                          s <- ident
                          return (Buffer s)
              remove = do skstring "remove"
                          return Remove 
              macro  = do skstring "macro"
                          st <- ident
                          skstring "{"
                          sc <- script 
                          skstring "}"
                          return (Macro st sc)
              call   = do st <- ident
                          return (Call st)

integer :: ReadP Int
integer = do skipSpaces 
             n <- many1 (satisfy isDigit)
             return $ read n

charN :: Int -> ReadP String 
charN i = do c <- many (satisfy isLatin1)
             if length c == i
                then return c
                else pfail

ident :: ReadP String 
ident = do skipSpaces
           cs <- munch(`elem` allowed)  
           if checkIdents cs && contains cs (['a'..'z']++['A'..'Z'])
              then return cs
              else pfail
        where allowed = ['a'..'z']++['A'..'Z']++['0'..'9']++"_./*?"
              checkIdents s = s `notElem` reservedKeywords 

contains :: String -> String -> Bool
contains []    _ = False
contains (h:t) l = contains' h l || contains t l  

contains' :: Char -> String -> Bool
contains' _ []     = False
contains' e (h:t)  = h == e || contains' e t

-----------------------------
--          Misc           --
-----------------------------

skstring :: String -> ReadP String
skstring st = skipSpaces >> string st

reservedKeywords :: [String]
reservedKeywords = ["i", "del", "next", "prev", "buffer", "remove", "macro"] 
