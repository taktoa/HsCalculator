{-# LANGUAGE TupleSections #-}
-- Main.hs
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

module Main where

import           Control.Monad   (unless)
import           Data.Functor    ((<$>))
import qualified Data.Map.Strict as M
import           Data.Text       (pack)
--import           System.IO           (hFlush, stdout)
import           Expr
import           Parse
import           PrettyPrint
import           Text.Parsec     (parse)

step :: (Context, Expr) -> (Context, Expr)
step (c, ERef n)                       = case M.lookup n c of
                                          Just a  -> (c, snd $ step (c, a))
                                          Nothing -> error $ "Referenced undefined variable: " ++ show n
step (c, ENeg (ERat 0))                = (c, ERat 0)
step (c, EMul (ERat 0) _)              = (c, ERat 0)
step (c, EMul _ (ERat 0))              = (c, ERat 0)
step (c, EAdd e (ERat 0))              = (c, e)
step (c, EAdd (ERat 0) e)              = (c, e)
step (c, EAdd (ERat a) (ERat b))       = (c, ERat (a + b))
step (_, ERcp (ERat 0))                = error "Divide by zero"
step (c, ERcp (ERat a))                = (c, ERat (recip a))
step (c, EMul (ERat a) (ERat b))       = (c, ERat (a * b))
step (c, EMul a b)                     = (c, EMul (snd $ eval' (c, a)) (snd $ step (c, b)))
step (c, EAdd a b)                     = (c, EAdd (snd $ eval' (c, a)) (snd $ step (c, b)))
step (c, EIf (ETF True) e _)           = (c, e)
step (c, EIf (ETF False) _ e)          = (c, e)
step (c, EIf b e1 e2)                  = (c, EIf (snd $ step (c, b)) e1 e2)
step (c, ELE (ERat a) (ERat b))        = (c, ETF (a <= b))
step (c, ELE a b)                      = step (c, ELE (snd $ step (c, a)) (snd $ step (c, b)))
step (c, EApp (ELam n r) i@(ERat _))   = (M.insert n i c, r)
step (c, EApp (ELam n r) i@(ELam _ _)) = (M.insert n i c, r)
step (c, EApp (ELam n r) i@(EMu _ _))  = (M.insert n i c, r)
step (c, EApp (ELam n r) i@(ETF _))    = (M.insert n i c, r)
step (c, EApp (EMu n r) b)             = (M.insert n (EMu n r) c, EApp r b)
step (c, EApp f b)                     = (c, EApp (snd $ step (c, f)) (snd $ step (c, b)))
step k@(_, ELam _ _)                   = k
step k@(_, EMu _ _)                    = k
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

testEval :: String -> Either String Expr
testEval = either (Left . show) (Right) . parse exprParse "stdin" . pack

testString :: String
testString = "(app (mu f (lam x (if (<= x 1) 1 (* (app f (+ x (- 1))))))) 4)"

main :: IO ()
main = putStrLn $ either id show $ testEval testString

-- main = do
--   let loop = do
--         putStr "==> "
--         hFlush stdout
--         r <- getLine
--         unless (invalid r) $ putStrLn (either id show $ testEval r) >> loop
--   loop
--   putStrLn "Goodbye!"
--   where
--     invalid x = x `elem` invalidList
--     invalidList = ["", "exit", "quit", ":q"]
