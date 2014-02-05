{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{---------------------------------------------------------------------
SalsaAst.  Haven't changed this except for SIdent and VIdent additions.
----------------------------------------------------------------------
Student name: Niels G. W. Serup <ngws@metanohi.name>
---------------------------------------------------------------------}
module SalsaAst where

type Program = [DefCom]
data DefCom = Def Definition
            | Com Command
            deriving (Show, Eq)
data Definition = Viewdef VIdent Expr Expr
                | Rectangle SIdent Expr Expr Expr Expr Colour
                | Circle SIdent Expr Expr Expr Colour
                | View VIdent
                | Group VIdent [VIdent]
                deriving (Show, Eq)
data Command = Move [SIdent] Pos
             | At Command Ident
             | Par Command Command
             deriving (Show, Eq)
data Pos = Abs Expr Expr
         | Rel Expr Expr
         deriving (Show, Eq)
data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Const Integer
          | Xproj SIdent
          | Yproj SIdent
          deriving (Show, Eq)
data Colour = Blue | Plum | Red | Green | Orange
            deriving (Show, Eq)
type Ident = String

type SIdent = Ident
type VIdent = Ident
