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

-- newtype MName = MName String
--              deriving (Eq, Ord, Read, Show)

-- type Name = String

-- data Expr = ELam MName Expr
--           | EMu  MName Expr
--           | EApp Expr  Expr
--           | ERef MName
--           | ERat Rational
--           | ETF  Bool
--           | ELE  Expr  Expr
--           | EIf  Expr  Expr Expr
--           | ENeg Expr
--           | EAdd Expr  Expr
--           | ERcp Expr
--           | EMul Expr  Expr
--             deriving (Eq, Show, Read)
