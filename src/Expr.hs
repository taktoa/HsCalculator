module Expr where

import           Data.Map.Strict (Map)

newtype MName = MName String
             deriving (Eq, Ord, Read, Show)

type Name = String

type Context = Map MName Expr

data Expr = ELam MName Expr
          | EMu  MName Expr
          | EApp Expr  Expr
          | ERef MName
          | ERat Rational
          | ETF  Bool
          | ELE  Expr  Expr
          | EIf  Expr  Expr Expr
          | ENeg Expr
          | EAdd Expr  Expr
          | ERcp Expr
          | EMul Expr  Expr
            deriving (Eq, Show, Read)
