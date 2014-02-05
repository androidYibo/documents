#!/usr/bin/env runghc
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{---------------------------------------------------------------------
Command line tool to run tests.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module Main (main) where

import qualified SalsaParserTests as P
import qualified SalsaInterpTests as I

main :: IO ()
main = do
  P.runMain
  I.runMain
