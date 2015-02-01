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
import qualified Text.Parsec.Expr   as Ex
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token  as Tok

data Op = Add | Multiply | Divide deriving (Eq, Show)

data Expr = Num Int
          | BinOp Op Expr Expr
          | Function Op [Expr]
            deriving (Eq, Show)

--funcParse :: Parser Expr
--funcParse = do


exampleInput :: String
exampleInput = "example.hc"

main :: IO ()
main = do
  input <- readFile exampleInput
  print (lines input)
  putStrLn "Hello World!"
