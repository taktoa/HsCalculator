-- main.rkt
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

import           Text.Parsec
import qualified Text.Parsec.Char   as Pcr
import qualified Text.Parsec.Expr   as Ex
import           Text.Parsec.String (Parser)

data Op = Add | Mul | Div | Sqrt deriving (Eq, Show)

data Expr = Num Int
          | UnaryOp Op Expr
          | BinOp Op Expr Expr
          | Function Op [Expr]
            deriving (Eq, Show)

integerParse :: Parser Expr
integerParse = do
  c <- many Pcr.digit
  return $ Num (read c :: Int)

addP :: Parser Op
addP = char '+' >> return Add

mulP :: Parser Op
mulP = char '*' >> return Mul

divP :: Parser Op
divP = char '/' >> return Div

sqrtP :: Parser Op
sqrtP = string "sqrt" >> return Sqrt

opParse :: Parser Op
opParse = addP <|> mulP <|> divP <|> sqrtP

exprParse :: Parser Expr
exprParse = do
  char '('
  op <- opParse
  args <- many (space >> (integerParse <|> exprParse))
  return (Function op args)

repBin :: Expr -> Expr
repBin (Num a) = Num a
repBin (Function Add [x, y])  = (BinOp Add (repBin x) (repBin y))
repBin (Function Add (x:y:r)) = (BinOp Add (repBin x) (repBin (Function Add (y:r))))
repBin (Function Mul [x, y])  = (BinOp Add (repBin x) (repBin y))
repBin (Function Mul (x:y:r)) = (BinOp Mul (repBin x) (repBin (Function Mul (y:r))))
repBin (Function Div [x, y])  = (BinOp Div (repBin x) (repBin y))
repBin (Function Sqrt [x])    = (UnaryOp Sqrt (repBin x))
repBin _                      = error "Parse error"

evaluate :: Expr -> Double
evaluate f@(Function _ _) = evaluate $ repBin f
evaluate (Num a) = fromIntegral a
evaluate (BinOp Add a b)  = (evaluate a) + (evaluate b)
evaluate (BinOp Mul a b)  = (evaluate a) * (evaluate b)
evaluate (BinOp Div a b)  = (evaluate a) / (evaluate b)
evaluate (UnaryOp Sqrt a) = sqrt (evaluate a)

exampleInput :: String
exampleInput = "example.hc"

main :: IO ()
main = do
  input <- readFile exampleInput
  let parse1 = map (parse exprParse "") (lines input)
  print parse1
  print (map (either show (show . evaluate)) parse1)
  putStrLn "Hello World!"
