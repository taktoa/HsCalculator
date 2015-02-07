module Eval where

import qualified Data.Map.Strict as M
import           Expr

step :: (Context, Expr) -> (Context, Expr)
step (c, ERef n)                       = case M.lookup n c of
                                          Just a  -> (c, a)
                                          Nothing -> error $ "Referenced undefined variable: " ++ show n
step (c, ENeg (ERat 0))                = (c, ERat 0)
step (c, ENeg (ERat i))                = (c, ERat (-i))
step (c, EMul (ERat 0) _)              = (c, ERat 0)
step (c, EMul _ (ERat 0))              = (c, ERat 0)
step (c, EAdd e (ERat 0))              = (c, e)
step (c, EAdd (ERat 0) e)              = (c, e)
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
step (c, EApp (ELam n r) i@(ERat _))   = (M.insert n i c, r)
step (c, EApp (ELam n r) i@(ELam _ _)) = (M.insert n i c, r)
step (c, EApp (ELam n r) i@(EMu _ _))  = (M.insert n i c, r)
step (c, EApp (ELam n r) i@(ETF _))    = (M.insert n i c, r)
step (c, EApp (EMu n r) i@(ERat _))    = (M.insert n (EMu n r) c, EApp r i)
step (c, EApp (EMu n r) i@(ETF _))     = (M.insert n (EMu n r) c, EApp r i)
step (c, EApp f b)                     = (c, EApp (snd $ eval' (c, f)) (snd $ eval' (c, b)))
step k                                 = k

eval' :: (Context, Expr) -> (Context, Expr)
eval' i
 | i == s      = i
 | otherwise   = eval' s
 where
   s = step i

eval :: Expr -> Rational
eval (ERat i) = i
eval e        = eval $ snd $ eval' (M.empty, e)
