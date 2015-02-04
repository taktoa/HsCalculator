{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeOperators    #-}
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

-- import           Control.Applicative ((*>), (<*>))
-- import           Data.Functor        ((<$), (<$>))
import           Data.Hashable    (hash)
import           Data.Map.Strict  (Map, union)
import qualified Data.Map.Strict  as M
--import           Data.Ratio (Rational)
import           Data.Text        (pack)
import           Text.Parsec
import           Text.Parsec.Text (Parser)

newtype MName = MName Int
             deriving (Eq, Ord, Read, Show)

type Name = String

type Context = Map MName Expr

data Expr = ELam MName Expr
          | EMu  MName Expr
          | EApp Expr  Expr
          | ERef MName
          | ERat Rational
          | ENeg Expr
          | EAdd Expr  Expr
          | ERcp Expr
          | EMul Expr  Expr
            deriving (Eq, Show, Read)

munge :: String -> MName
munge = MName . hash

step :: (Context, Expr) -> (Context, Expr)
step (c, ERef n)                 = case M.lookup n c of
                                     Just a  -> let (c', a') = step (c, a) in (c `union` c', a')
                                     Nothing -> error $ "Referenced undefined variable: " ++ show n
step (c, ENeg (ERat a))          = (c, ERat (-a))
step (c, EAdd (ERat a) (ERat b)) = (c, ERat (a + b))
step (c, ERcp (ERat 0))          = error "Divide by zero"
step (c, ERcp (ERat a))          = (c, ERat (recip a))
step (c, EMul (ERat a) (ERat b)) = (c, ERat (a * b))
step (c, EMul a b)               = step (c, EMul (snd $ step (c, a)) (snd $ step (c, b)))
step (c, EAdd a b)               = step (c, EAdd (snd $ step (c, a)) (snd $ step (c, b)))
step (c, EApp (ELam n r) b)      = step (M.insert n b c, r)
step (c, EApp (EMu n r) b)       = step (M.insert n (EMu n r) c, r)
step (c, EApp f b)               = (c `union` c', EApp f' b) where (c', f')  = step (c, f)
step (c, e)                      = (c, e)

eval' :: (Context, Expr) -> (Context, Expr)
eval' i
  | i == s      = i
  | otherwise   = eval' s
  where
    s = step i

eval :: Expr -> Rational
eval (ERat i) = i
eval e        = eval $ snd $ eval' (M.empty, e)

intParse :: Parser Expr
intParse = do
  c <- many digit
  return $ ERat $ fromIntegral (read c :: Integer)

ratParse :: Parser Expr
ratParse = do
  c <- many digit
  char '%'
  d <- many digit
  return $ ERat $ read (c ++ "%" ++ d)

data PFunc = PLam
           | PMu
           | PApp
           | PNeg
           | PAdd
           | PMul
           | PRcp

operators :: [(String, PFunc)]
operators = [("-",      PNeg),
             ("+",      PAdd),
             ("~",      PRcp),
             ("*",      PMul),
             ("app",    PApp),
             ("mu",     PMu),
             ("lam",    PLam)]

varParse :: Parser Expr
varParse = (ERef . munge) `fmap` many1 letter

funcParse :: Parser PFunc
funcParse = choice $ map genOpP operators
          where
            genOpP (s, a) = string s >> return a

exprParse :: Parser Expr
exprParse = do
  char '('
  func <- funcParse
  whitespace
  args <- argParse `sepBy` whitespace
  char ')'
  return (toExpr func args)
  where
    argParse = choice [exprParse, varParse, try ratParse, intParse]
    whitespace = many1 space

toExpr :: PFunc -> [Expr] -> Expr
toExpr PNeg [x]         = ENeg x
toExpr PAdd [x]         = x
toExpr PAdd (x:xs)      = EAdd x (toExpr PAdd xs)
toExpr PRcp [x]         = ERcp x
toExpr PMul [x]         = x
toExpr PMul (x:xs)      = EMul x (toExpr PMul xs)
toExpr PLam [ERef n, r] = ELam n r
toExpr PMu  [ERef n, r] = EMu n r
toExpr PApp [f, a]      = EApp f a
toExpr PApp (f:a:as)    = toExpr PApp $ EApp f a : as

testEval :: String -> Either ParseError Rational
testEval = fmap eval . parse exprParse "stdin" . pack

main :: IO ()
main = print $ testEval "(app (lam x (lam y (* y y x))) 3 4)"

-- main :: IO ()
-- main = do
--   let loop = do
--         putStr "==> "
--         hFlush stdout
--         r <- getLine
--         unless (invalid r) (putStrLn (interpreterS r) >> loop)
--   loop
--   putStrLn "Goodbye!"
--   where
--     invalid x = x `elem` invalidList
--     invalidList = ["", "exit", "quit", ":q"]
