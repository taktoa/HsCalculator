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

import           Control.Applicative (Applicative (..))
import           Control.Monad       (ap, liftM)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IM (empty, union)
import           Data.Map.Strict     (Map, empty, union)
import           Data.String         (IsString (..))
import           Data.Text           (Text, pack)

newtype Name = Name Text
             deriving (Eq, Ord, Read, Show)

instance IsString Name where
  fromString = Name . pack

type Env = Map Name Int

type Store = (Int, IntMap Expr)

data Expr = ELam Name  Expr
          | EMu  Name  Expr
          | EApp Expr  Expr
          | ERef Name
          | ERat Rational
          | ETF  Bool
          | ELE  Expr  Expr
          | EIf  Expr  Expr Expr
          | ENeg Expr
          | EAdd Expr  Expr
          | ERcp Expr
          | EMul Expr  Expr
            deriving (Eq, Show, Read)

data GClosure e = Clsr Env Store e
                deriving (Eq, Show, Read)

type Closure = GClosure Expr
type EClosure = GClosure ()

instance Functor GClosure where
    fmap = liftM

instance Applicative GClosure where
    pure  = return
    (<*>) = ap

instance Monad GClosure where
  return  = Clsr empty (0, IM.empty)
  a >>= f = Clsr (e' `union` e) (i' + i, IM.union s' s) x'
    where
      Clsr e  (i,  s)  x  = a
      Clsr e' (i', s') x' = f x
