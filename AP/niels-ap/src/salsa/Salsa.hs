{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{---------------------------------------------------------------------
General Salsa API.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module Salsa (
  parseString,
  parseFile,
  runProg,
  runString,
  runFile
  ) where

import Control.Applicative ((<$>))

import SalsaParser (parseString, parseFile)
import SalsaInterp (runProg)
import Gpx

runString :: Integer -> String -> Animation
runString fps s = runProg fps $ either (error . show) id $ parseString s

runFile :: Integer -> FilePath -> IO Animation
runFile fps p = runProg fps . either (error . show) id <$> parseFile p
