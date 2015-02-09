-- PrettyPrint.hs
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

module PrettyPrint where

import           Data.Map.Strict
import           Data.Ratio      (denominator, numerator)
import           Expr

prettyPrint' :: String -> [Expr] -> String
prettyPrint' s es = "(" ++ s ++ concatMap ((' ':) . prettyPrint) es ++ ")"

prettyPrint :: Expr -> String
prettyPrint (ELam n e)  = prettyPrint' "λ" [ERef n, e]
prettyPrint (EMu n e)   = prettyPrint' "μ" [ERef n, e]
prettyPrint (ERef n)    = case n of MName m -> m
prettyPrint (ERat x)    = case denominator x of
                           1 -> show $ numerator x
                           _ -> show x
prettyPrint (ETF b)     = show b
prettyPrint (EApp f e)  = prettyPrint' "app" [f, e]
prettyPrint (EIf b x y) = prettyPrint' "if" [b, x, y]
prettyPrint (EMul a b)  = prettyPrint' "*" [a, b]
prettyPrint (EAdd a b)  = prettyPrint' "+" [a, b]
prettyPrint (ELE a b)   = prettyPrint' "<=" [a, b]
prettyPrint (ERcp a)    = prettyPrint' "~" [a]
prettyPrint (ENeg a)    = prettyPrint' "-" [a]

contextPrint :: Context -> String
contextPrint c = concatMap (\(a, b) -> prettyPrint (ERef a) ++ " ~> " ++ prettyPrint b ++ "\n") $ toList c

cpPrint :: (Context, Expr) -> String
cpPrint (c, e) = contextPrint c ++ "\n" ++ prettyPrint e ++ "\n ------------------ \n"
