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

import           Control.Monad    (unless)
import           Data.Text        (Text, pack, unpack)
import           System.IO
import           Text.Parsec
import           Text.Parsec.Text (Parser)

data Func = Add
          | Mul
          | Lam
          | App
          deriving (Eq, Show)

data Expr = INum Int
          | Boolean Bool
          | Var Text
          | Branch Func [Expr]
          deriving (Eq, Show)

intParse :: Parser Expr
intParse = many digit >>= (\c -> return $ INum (read c :: Int))

boolParse :: Parser Expr
boolParse = tP <|> fP
            where
              tP = string "true"  >> return (Boolean True)
              fP = string "false" >> return (Boolean False)

dataParse :: Parser Expr
dataParse = foldl1 (<|>) [intParse, boolParse]

operators :: [(String, Func)]
operators = [("+",      Add),
             ("*",      Mul),
             ("apply",  App),
             ("lambda", Lam)]

varParse :: Parser Expr
varParse = many1 letter >>= (return . Var . pack)

funcParse :: Parser Func
funcParse = foldl1 (<|>) $ map genOpP operators
          where
            genOpP (s, a) = string s >> return a

exprParse :: Parser Expr
exprParse = do
  char '('
  func <- funcParse
  whitespace
  args <- argParse `sepBy` whitespace
  char ')'
  return (Branch func args)
  where
    argParse = exprParse <|> varParse <|> dataParse
    whitespace = many1 space

checkBound' :: [Text] -> Expr -> Bool
checkBound' bnd (Var v) = v `elem` bnd
checkBound' bnd (Branch App [Branch Lam [Var v, a], _]) = checkBound' (v:bnd) a
checkBound' bnd (Branch _ as) = all (checkBound' bnd) as
checkBound' _   _ = True

checkBound :: Expr -> Bool
checkBound = checkBound' []

-- checkLam :: Expr -> Bool
-- checkLam (Branch Lam [(Var v), a]) = True && checkBound
-- checkLam _ = False

-- checkApp :: Expr -> Bool
-- checkApp (Branch App [])

-- reduceLam :: Expr -> ([Text], Expr)

-- reduceApp :: Expr -> Expr
-- reduceApp (Branch App ((Branch Lam xs):a@(_:_))) = Branch App ((Branch Lam xs):a)
-- reduceApp _ = error "wrong expr"
-- repBin :: Expr -> Expr
-- repBin (Num a) = Num a
-- repBin (Function Add [x, y])  = BinOp Add (repBin x) (repBin y)
-- repBin (Function Add (x:y:r)) = BinOp Add (repBin x) (repBin (Function Add (y:r)))
-- repBin (Function Mul [x, y])  = BinOp Add (repBin x) (repBin y)
-- repBin (Function Mul (x:y:r)) = BinOp Mul (repBin x) (repBin (Function Mul (y:r)))
-- repBin (Function Div [x, y])  = BinOp Div (repBin x) (repBin y)
-- repBin (Function Sub [x, y])  = BinOp Sub (repBin x) (repBin y)
-- repBin (Function Sqrt [x])    = UnaryOp Sqrt (repBin x)
-- repBin _                      = error "Parse error"

-- evaluate :: Expr -> Double
-- evaluate f@(Function _ _) = evaluate $ repBin f
-- evaluate (Num a)          = fromIntegral a
-- evaluate (BinOp Add a b)  = evaluate a + evaluate b
-- evaluate (BinOp Sub a b)  = evaluate a - evaluate b
-- evaluate (BinOp Mul a b)  = evaluate a * evaluate b
-- evaluate (BinOp Div a b)  = evaluate a / evaluate b
-- evaluate (UnaryOp Sqrt a) = sqrt (evaluate a)

-- evalParse :: String -> String
-- evalParse = either show (show . evaluate) . parse exprParse "stdin" . head . map pack . lines

-- main :: IO ()
-- main = do
--   let loop = do
--         putStr "==> "
--         hFlush stdout
--         r <- getLine
--         unless (invalid r) (putStrLn (evalParse r) >> loop)
--   loop
--   putStrLn "Goodbye!"
--   where
--     invalid x = x `elem` invalidList
--     invalidList = ["", "exit", "quit", ":q"]

main :: IO ()
main = putStrLn "test"
