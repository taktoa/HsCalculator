-- Parse.hs
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

module Parse where

--import           Control.Applicative ((*>), (<*), (<**>), (<*>))
import           Data.Functor     ((<$), (<$>))
--import           Data.Hashable       (hash)
import           Text.Parsec
import           Text.Parsec.Text (Parser)
--import           Data.Ratio          ((%))
import           Expr

munge :: String -> MName
munge = MName

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

boolParse :: Parser Expr
boolParse = ETF True <$ trueParse <|> ETF False <$ falseParse
  where
    trueParse  = string "true"
    falseParse = string "false"

data PFunc = PLam
           | PMu
           | PApp
           | PIf
           | PEQ
           | PLT
           | PGT
           | PLE
           | PGE
           | PNot
           | POr
           | PAnd
           | PNeg
           | PAdd
           | PMul
           | PRcp
           deriving (Eq, Show, Read)

data EvalError = UndefinedVariableError String
               | DivideByZeroError
               deriving (Eq, Show, Read)

varParse :: Parser Expr
varParse = (ERef . munge) `fmap` many1 letter

operators :: [(String, PFunc)]
operators = [("-",      PNeg),
             ("+",      PAdd),
             ("~",      PRcp),
             ("*",      PMul),
             ("if",     PIf),
             ("=",      PEQ),
             ("<=",     PLE),
             (">=",     PGE),
             ("<",      PLT),
             (">",      PGT),
             ("!",      PNot),
             ("&",      PAnd),
             ("|",      POr),
             ("app",    PApp),
             ("mu",     PMu),
             ("lam",    PLam)]

funcParse :: Parser PFunc
funcParse = choice $ map genOpP operators
  where
    genOpP (s, a) = try (string s >> return a)

sexpParse :: Parser a -> Parser b -> Parser (a, [b])
sexpParse p1 p2 = do
  char '('
  spaces
  f <- p1
  spaces1
  as <- p2 `sepBy` spaces1
  spaces
  char ')'
  return (f, as)
  where
    spaces1 = space >> spaces

exprParse :: Parser Expr
exprParse = uncurry toExpr <$> sexpParse funcParse argParse
  where
    argParse = choice [try boolParse, exprParse, varParse, try ratParse, intParse]

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
toExpr f a              = desugar f a

desugar :: PFunc -> [Expr] -> Expr
desugar POr  [a, b]      = EIf a (ETF True) b
desugar PAnd [a, b]      = EIf a b (ETF False)
desugar PNot [a]         = EIf a (ETF False) (ETF True)
desugar PEQ  [a, b]      = toExpr PAnd [toExpr PLE [a, b], toExpr PNot [toExpr PLT [a, b]]]
desugar PLT  [a, b]      = toExpr PNot [toExpr PGE [a, b]]
desugar PGT  [a, b]      = toExpr PNot [toExpr PLE [a, b]]
desugar PIf  [b, t, f]   = EIf b t f
desugar PLE  [a, b]      = ELE a b
desugar PGE  [a, b]      = ELE b a
