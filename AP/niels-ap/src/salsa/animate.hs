#!/usr/bin/env runghc
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{---------------------------------------------------------------------
Command line tool to generate animations.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module Main (main) where

import Control.Monad
import System.Environment

import Salsa


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path, fps] -> printAni =<< runFile (read fps) path
    [path] -> printAni =<< runFile 10 path
    _ -> putStrLn "usage: animate.hs FILEPATH [FPS (default is 10)]"
  where printAni (a, b) = do
          print a
          putStrLn ""
          if null b
            then putStrLn "[]"
            else do 
            putStr "[ " >> print (head b)
            forM_ (tail b) $ \x -> putStr ", " >> print x
            putStrLn "]"
