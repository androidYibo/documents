module EddyParserTest where

import Test.QuickCheck as QC

import EddyParser
import EddyAst


newtype PosInt = PosInt Integer deriving (Eq, Show)
instance QC.Arbitrary PosInt where
  arbitrary = do 
    (QC.NonNegative n) <- QC.arbitrary
    return $ PosInt n

idents = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ["_./*?"]

newtype Ident = Ident deriving (Eq, Show)
instance QC.Arbitrary Ident where
  arbitrary = do
    s <- QC.arbitrary  
