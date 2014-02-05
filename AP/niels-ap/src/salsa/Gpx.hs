{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{---------------------------------------------------------------------
Gpx.  Haven't changed this.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module Gpx where

type ViewName = String
type ColourName = String
type Frame = [GpxInstr]
type Animation = ([(ViewName, Integer, Integer)], [Frame])
data GpxInstr = DrawRect Integer Integer Integer Integer ViewName ColourName
              | DrawCirc Integer Integer Integer ViewName ColourName
              deriving (Eq, Show, Ord)
