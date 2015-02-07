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
toExpr POr  [a, b]      = EIf a (ETF True) b
toExpr PAnd [a, b]      = EIf a b (ETF False)
toExpr PNot [a]         = EIf a (ETF False) (ETF True)
toExpr PEQ  [a, b]      = toExpr PAnd [toExpr PLE [a, b], toExpr PNot [toExpr PLT [a, b]]]
toExpr PLT  [a, b]      = toExpr PNot [toExpr PGE [a, b]]
toExpr PGT  [a, b]      = toExpr PNot [toExpr PLE [a, b]]
toExpr PIf  [b, t, f]   = EIf b t f
toExpr PLE  [a, b]      = ELE a b
toExpr PGE  [a, b]      = ELE b a
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
