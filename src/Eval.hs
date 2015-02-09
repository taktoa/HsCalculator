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

import           Data.Map.Strict (empty, insert)
import qualified Data.Map.Strict as M (lookup)
import           Expr

step :: (Context, Expr) -> (Context, Expr)
step (c, ERef n)                       = case M.lookup n c of
                                          Just a  -> (c, a)
                                          Nothing -> error $ "Referenced undefined variable: " ++ show n
step (c, ENeg (ERat i))                = (c, ERat (-i))
step (c, EAdd (ERat a) (ERat b))       = (c, ERat (a + b))
step (_, ERcp (ERat 0))                = error "Divide by zero"
step (c, ERcp (ERat a))                = (c, ERat (recip a))
step (c, EMul (ERat a) (ERat b))       = (c, ERat (a * b))
step (c, EMul a b)                     = (c, EMul (snd $ eval' (c, a)) (snd $ eval' (c, b)))
step (c, EAdd a b)                     = (c, EAdd (snd $ eval' (c, a)) (snd $ eval' (c, b)))
step (c, EIf (ETF True) e _)           = (c, e)
step (c, EIf (ETF False) _ e)          = (c, e)
step (c, EIf b e1 e2)                  = (c, EIf (snd $ eval' (c, b)) e1 e2)
step (c, ELE (ERat a) (ERat b))        = (c, ETF (a <= b))
step (c, ELE a b)                      = (c, ELE (snd $ eval' (c, a)) (snd $ eval' (c, b)))
step (c, EApp (ELam n r) i@(ERat _))   = (insert n i c, r)
step (c, EApp (ELam n r) i@(ELam _ _)) = (insert n i c, r)
step (c, EApp (ELam n r) i@(EMu _ _))  = (insert n i c, r)
step (c, EApp (ELam n r) i@(ETF _))    = (insert n i c, r)
step (c, EApp (EMu n r) i@(ERat _))    = (insert n (EMu n r) c, EApp r i)
step (c, EApp (EMu n r) i@(ETF _))     = (insert n (EMu n r) c, EApp r i)
step (c, EApp f b)                     = (c, EApp (snd $ eval' (c, f)) (snd $ eval' (c, b)))
step k                                 = k

eval' :: (Context, Expr) -> (Context, Expr)
eval' i
 | i == s      = i
 | otherwise   = eval' s
 where
   s = step i

eval :: Expr -> Expr
eval = snd . eval' . (empty,)
