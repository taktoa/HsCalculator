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
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as M
import           Data.Text        (Text, pack, unpack)
import           System.IO
import           Text.Parsec
import           Text.Parsec.Text (Parser)

type VarName = Text

data Func  = Add
           | Mul
           | Lam
           | App
           deriving (Eq, Show)

data Value = INum Int
           | Boolean Bool
           deriving (Eq, Show)

data Expr  = Val Value
           | Var VarName
           | Branch Func [Expr]
           deriving (Eq, Show)

intParse :: Parser Value
intParse = many digit >>= (\c -> return $ INum (read c :: Int))

boolParse :: Parser Value
boolParse = tP <|> fP
            where
              tP = string "true"  >> return (Boolean True)
              fP = string "false" >> return (Boolean False)

dataParse :: Parser Expr
dataParse = Val `fmap` foldl1 (<|>) [intParse, boolParse]

operators :: [(String, Func)]
operators = [("+",      Add),
             ("*",      Mul),
             ("apply",  App),
             ("lambda", Lam)]

varParse :: Parser Expr
varParse = (Var . pack) `fmap` many1 letter

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

checkValid' :: [VarName] -> Expr -> Bool
checkValid' _   (Val _) = True
checkValid' bnd (Var v) = v `elem` bnd
checkValid' bnd (Branch App as@[_, _]) = all (checkValid' bnd) as
checkValid' _   (Branch App _) = False
checkValid' bnd (Branch Lam [Var v, a]) = checkValid' (v:bnd) a
checkValid' _   (Branch Lam _) = False
checkValid' bnd (Branch _ as) = all (checkValid' bnd) as

checkValid :: Expr -> Bool
checkValid = checkValid' []

data EvalError = TypeError String
               | UnboundVarError String
               deriving (Eq, Show)

data VType = BT | IT deriving (Eq, Show)

type EValue = Either [EvalError] Value

type Context = Map VarName (Value, VType)

getType :: Value -> VType
getType (Boolean _) = BT
getType (INum _) = IT

unboundError :: String -> EvalError
unboundError vn = UnboundVarError ("Variable " ++ vn ++ " is not bound")

typeError :: String -> String -> String -> EvalError
typeError f a b = TypeError ("Type error: (" ++ f ++ " " ++ a ++ " " ++ b ++ ")")

addVals :: EValue -> EValue -> EValue
addVals (Right (INum v1)) (Right (INum v2)) = Right (INum (v1 + v2))
addVals (Left e1)         (Left e2)         = Left (e1 ++ e2)
addVals (Left e1)         _                 = Left e1
addVals _                 (Left e2)         = Left e2
addVals x                 y                 = Left [typeError "+" (show x) (show y)]

mulVals :: EValue -> EValue -> EValue
mulVals (Right (INum 0))  _                 = Right (INum 0)
mulVals _                 (Right (INum 0))  = Right (INum 0)
mulVals (Right (INum v1)) (Right (INum v2)) = Right (INum (v1 * v2))
mulVals (Left e1)         (Left e2)         = Left (e1 ++ e2)
mulVals (Left e1)         _                 = Left e1
mulVals _                 (Left e2)         = Left e2
mulVals x                 y                 = Left [typeError "*" (show x) (show y)]

evaluate' :: Context -> Expr -> EValue
evaluate' _   (Val a)         = Right a
evaluate' ctx (Var a)         = maybe (Left [unboundError $ unpack a]) (Right . fst) $ M.lookup a ctx
evaluate' _   (Branch Add []) = Right (INum 0)
evaluate' ctx (Branch Add xs) = foldl1 addVals $ map (evaluate' ctx) xs
evaluate' _   (Branch Mul []) = Right (INum 1)
evaluate' ctx (Branch Mul xs) = foldl1 mulVals $ map (evaluate' ctx) xs
evaluate' ctx (Branch App [Branch Lam [Var v, e], Val b])
                              = evaluate' (M.insert v (b, getType b) ctx) e
evaluate' _ _                 = error "undefined behavior"

evaluate :: Expr -> EValue
evaluate = evaluate' M.empty

interpreter' :: Text -> Text
interpreter' x = case parse exprParse "parser" x of
  Left err -> pack $ show err
  Right p  -> if checkValid p then pack $ render $ evaluate p else "Syntax error"
  where
    render (Left xs) = foldl1 (\a b -> a ++ "\n" ++ b) $ map show xs
    render (Right a) = show a

interpreterS :: String -> String
interpreterS = unpack . interpreter' . pack

main :: IO ()
main = do
  let loop = do
        putStr "==> "
        hFlush stdout
        r <- getLine
        unless (invalid r) (putStrLn (interpreterS r) >> loop)
  loop
  putStrLn "Goodbye!"
  where
    invalid x = x `elem` invalidList
    invalidList = ["", "exit", "quit", ":q"]
