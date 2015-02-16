-- Eval.hs
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

module Eval where

import qualified Data.IntMap.Strict as IM (insert, lookup)
import qualified Data.Map.Strict    as M (insert, lookup)
import           Data.Maybe         (fromMaybe)
import           Expr

addVar :: EClosure -> Name -> Expr -> EClosure
addVar (Clsr e (addr, s) ()) n v = Clsr e' s' ()
  where
    addr' = addr + 1
    e' = M.insert n addr' e
    s' = (addr', IM.insert addr' v s)

refVar :: EClosure -> Name -> Expr
refVar (Clsr e (_, s) ()) n = fromMaybe err $ M.lookup n e >>= flip IM.lookup s
  where
    err = error ("Undefined variable: " ++ show n)

applyLam :: EClosure -> Name -> Expr -> Expr -> Closure
applyLam c n r v = wrap c' r
  where
    c' = addVar c n v

applyMu :: EClosure -> Name -> Expr -> Expr -> Closure
applyMu c n r v = wrap c' (EApp r v)
  where
    c' = addVar c n (EMu n r)

step' :: EClosure -> Expr -> Closure
--- Base cases for stepping arithmetic expressions
step' c (ENeg (ERat i))            = wrap c $ ERat (-i)
step' c (ERcp (ERat i))            = wrap c $ ERat (recip i)
step' c (EAdd (ERat i1) (ERat i2)) = wrap c $ ERat (i1 + i2)
step' c (EMul (ERat i1) (ERat i2)) = wrap c $ ERat (i1 * i2)
--- Base cases for booleans and conditionals
step' c (ELE  (ERat i1) (ERat i2)) = wrap c $ ETF (i1 <= i2)
step' c (EIf (ETF True) e1 _)      = wrap c $ e1
step' c (EIf (ETF False) _ e2)     = wrap c $ e2
--- Base cases for variable lookup
step' c (ERef n)                   = wrap c $ refVar c n
--- Lambda application
step' c (EApp (ELam n r) v)        = case v of
                                      ERat{} -> applyLam c n r v
                                      ETF{}  -> applyLam c n r v
                                      ELam{} -> applyLam c n r v
                                      EMu{}  -> applyLam c n r v
                                      _      -> wrap c $ EApp (ELam n r) (fwrap c v)
--- Mu application
step' c (EApp (EMu n r) v)         = case v of
                                      ERat{} -> applyMu c n r v
                                      ETF{}  -> applyMu c n r v
                                      ELam{} -> applyMu c n r v
                                      EMu{}  -> applyMu c n r v
                                      _      -> wrap c $ EApp (EMu n r) (fwrap c v)
--- Strictness cases
step' c (ENeg e)                   = wrap c $ ENeg (fwrap c e)
step' c (ERcp e)                   = wrap c $ ERcp (fwrap c e)
step' c (EAdd e1 e2)               = wrap c $ EAdd (fwrap c e1) (fwrap c e2)
step' c (EMul e1 e2)               = wrap c $ EMul (fwrap c e1) (fwrap c e2)
step' c (ELE e1 e2)                = wrap c $ ELE (fwrap c e1) (fwrap c e2)
step' c (EIf b e1 e2)              = wrap c $ EIf (fwrap c b) e1 e2
step' c (EApp a v)                 = wrap c $ EApp (fwrap c a) v
--- Don't reduce anything that cannot be reduced
step' c e                          = wrap c $ e

step :: Closure -> Closure
step (Clsr e s v) = step' (Clsr e s ()) v

eval' :: Closure -> Closure
eval' i
 | i == s      = s
 | otherwise   = eval' s
 where
   s = step i

force :: Closure -> Expr
force = getExpr . eval'

wrap :: GClosure a -> Expr -> Closure
wrap (Clsr e s _) = Clsr e s

fwrap :: GClosure a -> Expr -> Expr
fwrap c = force . wrap c

getExpr :: Closure -> Expr
getExpr (Clsr _ _ e) = e

eval :: Expr -> Expr
eval = force . return

