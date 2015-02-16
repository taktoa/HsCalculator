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

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M
import           Data.Ratio         (denominator, numerator)
import           Data.Text          (unpack)
import           Expr

prettyPrint' :: String -> [Expr] -> String
prettyPrint' s es = "(" ++ s ++ concatMap ((' ':) . prettyPrint) es ++ ")"

prettyPrint :: Expr -> String
prettyPrint (ELam n e)  = prettyPrint' "lam" [ERef n, e]
prettyPrint (EMu n e)   = prettyPrint' "mu" [ERef n, e]
prettyPrint (ERef n)    = case n of Name m -> unpack m
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

mapPrint :: (a -> String) -> (b -> String) -> [(a, b)] -> String
mapPrint f g = concatMap (\(a, b) -> f a ++ " ~> " ++ g b ++ "\n")

envPrint :: Env -> String
envPrint = mapPrint (\(Name n) -> unpack n) show . M.toList

storePrint :: Store -> String
storePrint = mapPrint show prettyPrint . IM.toList . snd

cpPrint :: Closure -> String
cpPrint (Clsr e s x) = envPrint e ++ "\n" ++ storePrint s ++ "\n" ++ prettyPrint x ++ "\n ------------------ \n"
