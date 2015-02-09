-- Expr.hs
-- Copyright 2015 Remy E. Goldschmidt <taktoa@gmail.com>
-- This file is part of HsCalculator.
--    HsCalculator is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    HsCalculator is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY-- without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with HsCalculator. If not, see <http://www.gnu.org/licenses/>.

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
