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

import           Data.Hashable    (hash)
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as M
import           Data.Text        (Text, pack, unpack)
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
          | EInt Int
          | EAdd Expr  Expr
          | EMul Expr  Expr
            deriving (Eq, Show, Read)

munge :: String -> MName
munge = MName . hash

step :: (Context, Expr) -> (Context, Expr)
step (c, ERef n)                 = case M.lookup n c of
                                     Just a  -> (c, a)
                                     Nothing -> error $ "Referenced undefined variable: " ++ show n
step (c, EMul (EInt a) (EInt b)) = (c, EInt (a * b))
step (c, EAdd (EInt a) (EInt b)) = (c, EInt (a + b))
step (c, EMul a b)               = step (c, EMul (snd $ step (c, a)) (snd $ step (c, b)))
step (c, EAdd a b)               = step (c, EAdd (snd $ step (c, a)) (snd $ step (c, b)))
step (c, EApp (ELam n r) b)      = step (M.insert n b c, r)
step (c, EApp (EMu n r) b)       = step (M.insert n (EMu n r) c, b)
step (c, EApp f b)               = (c `M.union` c', EApp f' b) where (c', f')  = step (c, f)
step (c, e)                      = (c, e)

eval' :: (Context, Expr) -> (Context, Expr)
eval' i
  | i == s      = i
  | otherwise   = eval' s
  where
    s = step i

eval :: Expr -> Int
eval (EInt i) = i
eval e        = eval $ snd $ eval' (M.empty, e)

intParse :: Parser Expr
intParse = many digit >>= (\c -> return $ EInt (read c :: Int))

data PFunc = PAdd | PMul | PLam | PMu | PApp

operators :: [(String, PFunc)]
operators = [("+",      PAdd),
             ("*",      PMul),
             ("app",    PApp),
             ("mu",     PMu),
             ("lam",    PLam)]

varParse :: Parser Expr
varParse = (ERef . munge) `fmap` many1 letter

funcParse :: Parser PFunc
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
  return (toExpr func args)
  where
    argParse = exprParse <|> varParse <|> intParse
    whitespace = many1 space

toExpr :: PFunc -> [Expr] -> Expr
toExpr PAdd [x]         = x
toExpr PAdd (x:xs)      = EAdd x (toExpr PAdd xs)
toExpr PMul [x]         = x
toExpr PMul (x:xs)      = EMul x (toExpr PMul xs)
toExpr PLam [ERef n, r] = ELam n r
toExpr PMu  [ERef n, r] = EMu n r
toExpr PApp [f, a]      = EApp f a
toExpr PApp (f:a:as)    = toExpr PApp $ EApp f a : as

testEval :: String -> Either ParseError Int
testEval = fmap eval . parse exprParse "stdin" . pack

main :: IO ()
main = print $ testEval "(app (lam x (lam y (* y y x))) 3 4)"

-- checkValid' :: [VarName] -> Expr -> Bool
-- checkValid' _   (Val _) = True
-- checkValid' bnd (Var v) = v `elem` bnd
-- checkValid' bnd (Branch App as@[_, _]) = all (checkValid' bnd) as
-- checkValid' _   (Branch App _) = False
-- checkValid' bnd (Branch Lam [Var v, a]) = checkValid' (v:bnd) a
-- checkValid' _   (Branch Lam _) = False
-- checkValid' bnd (Branch _ as) = all (checkValid' bnd) as

-- checkValid :: Expr -> Bool
-- checkValid = checkValid' []

-- data EvalError = TypeError String
--                | UnboundVarError String
--                deriving (Eq, Show)

-- data VType = BT | IT deriving (Eq, Show)

-- type EValue = Either [EvalError] Value

-- type Context = Map VarName (Value, VType)

-- getType :: Value -> VType
-- getType (Boolean _) = BT
-- getType (INum _) = IT

-- unboundError :: String -> EvalError
-- unboundError vn = UnboundVarError ("Variable " ++ vn ++ " is not bound")

-- typeError :: String -> String -> String -> EvalError
-- typeError f a b = TypeError ("Type error: (" ++ f ++ " " ++ a ++ " " ++ b ++ ")")

-- addVals :: EValue -> EValue -> EValue
-- addVals (Right (INum v1)) (Right (INum v2)) = Right (INum (v1 + v2))
-- addVals (Left e1)         (Left e2)         = Left (e1 ++ e2)
-- addVals (Left e1)         _                 = Left e1
-- addVals _                 (Left e2)         = Left e2
-- addVals x                 y                 = Left [typeError "+" (show x) (show y)]

-- mulVals :: EValue -> EValue -> EValue
-- mulVals (Right (INum 0))  _                 = Right (INum 0)
-- mulVals _                 (Right (INum 0))  = Right (INum 0)
-- mulVals (Right (INum v1)) (Right (INum v2)) = Right (INum (v1 * v2))
-- mulVals (Left e1)         (Left e2)         = Left (e1 ++ e2)
-- mulVals (Left e1)         _                 = Left e1
-- mulVals _                 (Left e2)         = Left e2
-- mulVals x                 y                 = Left [typeError "*" (show x) (show y)]

-- evaluate' :: Context -> Expr -> EValue
-- evaluate' _   (Val a)         = Right a
-- evaluate' ctx (Var a)         = maybe (Left [unboundError $ unpack a]) (Right . fst) $ M.lookup a ctx
-- evaluate' _   (Branch Add []) = Right (INum 0)
-- evaluate' ctx (Branch Add xs) = foldl1 addVals $ map (evaluate' ctx) xs
-- evaluate' _   (Branch Mul []) = Right (INum 1)
-- evaluate' ctx (Branch Mul xs) = foldl1 mulVals $ map (evaluate' ctx) xs
-- evaluate' ctx (Branch App [Branch Lam [Var v, e], Val b])
--                               = evaluate' (M.insert v (b, getType b) ctx) e
-- evaluate' _ _                 = error "undefined behavior"

-- evaluate :: Expr -> EValue
-- evaluate = evaluate' M.empty

-- interpreter' :: Text -> Text
-- interpreter' x = case parse exprParse "parser" x of
--   Left err -> pack $ show err
--   Right p  -> if checkValid p then pack $ render $ evaluate p else "Syntax error"
--   where
--     render (Left xs) = foldl1 (\a b -> a ++ "\n" ++ b) $ map show xs
--     render (Right a) = show a

-- interpreterS :: String -> String
-- interpreterS = unpack . interpreter' . pack

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
















-- data Lam :: * -> * where
--   Lift :: a                -> Lam a
--   Abs  :: (Lam a -> Lam b) -> Lam (a -> b)
--   App  :: Lam (a -> b)     -> Lam a        -> Lam b
--   Fix  :: Lam (a -> a)     -> Lam a

-- eval :: Expr a -> a
-- eval (EInt  a)   = a
-- eval (EBool a)   = a
-- eval (EAdd a b)  = eval a + eval b
-- eval (EMul a b)  = eval a * eval b
-- eval (EIf c a b) = if eval c then eval a else eval b
-- eval (ELT a b)   = eval a < eval b
-- eval (ENand a b) = not (eval a && eval b)

-- eval2 :: Lam a -> a
-- eval2 (Lift v)  = v
-- eval2 (Abs f)   = eval2 . f . Lift
-- eval2 (App f i) = eval2 f (eval2 i)
-- eval2 (Fix f)   = eval2 f (eval2 $ Fix f)

-- data Expr f = In (f (Expr f))
-- data Val e = Val Int
-- data Add e = Add e e
-- type IntExpr = Expr Val
-- type AddExpr = Expr Add

-- data (f :+: g) e = InL (f e) | InR (g e)

-- instance Functor Val where
--   fmap f (Val x) = Val x

-- instance Functor Add where
--   fmap f (Add e1 e2) = Add (f e1) (f e2)

-- class Functor f => Eval f where
--   evalAlgebra :: f Int -> Int

-- instance Eval Val where
--   evalAlgebra (Val x) = x

-- instance Eval Add where
--   evalAlgebra (Add x y) = x + y

-- instance (Functor f, Functor g) => Functor (f :+: g) where
--   fmap f (InL e1) = InL (fmap f e1)
--   fmap f (InR e2) = InR (fmap f e2)

-- instance (Eval f, Eval g) => Eval (f :+: g) where
--   evalAlgebra (InL x) = evalAlgebra x
--   evalAlgebra (InR y) = evalAlgebra y

-- foldExpr :: Functor f => (f a -> a) -> Expr f -> a
-- foldExpr f (In t) = f (fmap (foldExpr f) t)

-- eval :: Eval f => Expr f -> Int
-- eval = foldExpr evalAlgebra

-- class (Functor sub, Functor sup) => sub :<: sup where
--   inj :: sub a -> sup a

-- instance Functor f => f :<: f where
--   inj = id

-- instance (Functor f, Functor g) => f :<: (f :+: g) where
--   inj = InL

-- instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
--   inj = InR . inj

-- inject :: (g :<: f) => g (Expr f) -> Expr f
-- inject = In . inj

-- val :: (Val :<: f) => Int -> Expr f
-- val x = inject (Val x)

-- (+++) :: (Add :<: f) => Expr f -> Expr f -> Expr f
-- x +++ y = inject (Add x y)

-- infixl 6 +++

-- data Mul x = Mul x x
-- instance Functor Mul where
--   fmap f (Mul x y) = Mul (f x) (f y)
-- instance Eval Mul where
--   evalAlgebra (Mul x y) = x * y
-- infixl 7 ***
-- (***) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
-- x *** y = inject (Mul x y)

-- testAdd :: Expr ((Val :+: Mul) :+: Add)
-- testAdd = ((val 80) *** (val 5)) +++ (val 4)

-- main :: IO ()
-- main = print $ eval testAdd

--------------------------------------

-- type VarName = Text

-- data Func  = Add
--            | Mul
--            | Lam
--            | App
--            deriving (Eq, Show)

-- data Value = INum Int
--            | Boolean Bool
--            deriving (Eq, Show)

-- data Expr  = Val Value
--            | Var VarName
--            | Branch Func [Expr]
--            deriving (Eq, Show)

-- intParse :: Parser Value
-- intParse = many digit >>= (\c -> return $ INum (read c :: Int))

-- boolParse :: Parser Value
-- boolParse = tP <|> fP
--             where
--               tP = string "true"  >> return (Boolean True)
--               fP = string "false" >> return (Boolean False)

-- dataParse :: Parser Expr
-- dataParse = Val `fmap` foldl1 (<|>) [intParse, boolParse]

-- operators :: [(String, Func)]
-- operators = [("+",      Add),
--              ("*",      Mul),
--              ("apply",  App),
--              ("lambda", Lam)]

-- varParse :: Parser Expr
-- varParse = (Var . pack) `fmap` many1 letter

-- funcParse :: Parser Func
-- funcParse = foldl1 (<|>) $ map genOpP operators
--           where
--             genOpP (s, a) = string s >> return a

-- exprParse :: Parser Expr
-- exprParse = do
--   char '('
--   func <- funcParse
--   whitespace
--   args <- argParse `sepBy` whitespace
--   char ')'
--   return (Branch func args)
--   where
--     argParse = exprParse <|> varParse <|> dataParse
--     whitespace = many1 space

-- checkValid' :: [VarName] -> Expr -> Bool
-- checkValid' _   (Val _) = True
-- checkValid' bnd (Var v) = v `elem` bnd
-- checkValid' bnd (Branch App as@[_, _]) = all (checkValid' bnd) as
-- checkValid' _   (Branch App _) = False
-- checkValid' bnd (Branch Lam [Var v, a]) = checkValid' (v:bnd) a
-- checkValid' _   (Branch Lam _) = False
-- checkValid' bnd (Branch _ as) = all (checkValid' bnd) as

-- checkValid :: Expr -> Bool
-- checkValid = checkValid' []

-- data EvalError = TypeError String
--                | UnboundVarError String
--                deriving (Eq, Show)

-- data VType = BT | IT deriving (Eq, Show)

-- type EValue = Either [EvalError] Value

-- type Context = Map VarName (Value, VType)

-- getType :: Value -> VType
-- getType (Boolean _) = BT
-- getType (INum _) = IT

-- unboundError :: String -> EvalError
-- unboundError vn = UnboundVarError ("Variable " ++ vn ++ " is not bound")

-- typeError :: String -> String -> String -> EvalError
-- typeError f a b = TypeError ("Type error: (" ++ f ++ " " ++ a ++ " " ++ b ++ ")")

-- addVals :: EValue -> EValue -> EValue
-- addVals (Right (INum v1)) (Right (INum v2)) = Right (INum (v1 + v2))
-- addVals (Left e1)         (Left e2)         = Left (e1 ++ e2)
-- addVals (Left e1)         _                 = Left e1
-- addVals _                 (Left e2)         = Left e2
-- addVals x                 y                 = Left [typeError "+" (show x) (show y)]

-- mulVals :: EValue -> EValue -> EValue
-- mulVals (Right (INum 0))  _                 = Right (INum 0)
-- mulVals _                 (Right (INum 0))  = Right (INum 0)
-- mulVals (Right (INum v1)) (Right (INum v2)) = Right (INum (v1 * v2))
-- mulVals (Left e1)         (Left e2)         = Left (e1 ++ e2)
-- mulVals (Left e1)         _                 = Left e1
-- mulVals _                 (Left e2)         = Left e2
-- mulVals x                 y                 = Left [typeError "*" (show x) (show y)]

-- evaluate' :: Context -> Expr -> EValue
-- evaluate' _   (Val a)         = Right a
-- evaluate' ctx (Var a)         = maybe (Left [unboundError $ unpack a]) (Right . fst) $ M.lookup a ctx
-- evaluate' _   (Branch Add []) = Right (INum 0)
-- evaluate' ctx (Branch Add xs) = foldl1 addVals $ map (evaluate' ctx) xs
-- evaluate' _   (Branch Mul []) = Right (INum 1)
-- evaluate' ctx (Branch Mul xs) = foldl1 mulVals $ map (evaluate' ctx) xs
-- evaluate' ctx (Branch App [Branch Lam [Var v, e], Val b])
--                               = evaluate' (M.insert v (b, getType b) ctx) e
-- evaluate' _ _                 = error "undefined behavior"

-- evaluate :: Expr -> EValue
-- evaluate = evaluate' M.empty

-- interpreter' :: Text -> Text
-- interpreter' x = case parse exprParse "parser" x of
--   Left err -> pack $ show err
--   Right p  -> if checkValid p then pack $ render $ evaluate p else "Syntax error"
--   where
--     render (Left xs) = foldl1 (\a b -> a ++ "\n" ++ b) $ map show xs
--     render (Right a) = show a

-- interpreterS :: String -> String
-- interpreterS = unpack . interpreter' . pack

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
