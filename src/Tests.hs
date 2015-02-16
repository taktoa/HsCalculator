-- Tests.hs
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

module Tests where

import           Control.Monad                     (liftM, liftM2, replicateM)
import           Data.Functor                      ((<$>))
import           Data.Map.Strict                   (empty)
import           Data.Ratio                        (denominator, numerator)
import           Data.Text                         (Text, pack, unpack)
import           Debug.Trace                       (trace)
import           Distribution.TestSuite
import           Distribution.TestSuite.QuickCheck
import           Eval
import           Expr
import           Parse
import           Test.QuickCheck
import           Text.Parsec                       (ParseError, parse)

-- Helper functions

ratToInt :: Rational -> Integer
ratToInt r = numerator r `div` denominator r

showR :: Rational -> String
showR r = show (numerator r) ++ "%" ++ show (denominator r)

exprParser :: String -> Either ParseError Expr
exprParser = parse exprParse "test" . pack

evaluate :: String -> Rational
evaluate s = case either perr eval $ exprParser s of
              ERat i -> i
              e      -> eerr e
  where
    throw m e = error $ m ++ show e
    perr = throw "Parse error in evaluate: "
    eerr = throw "Evaluation error in evaluate: "

applyStr :: String -> [String] -> String
applyStr f as = "(" ++ f ++ concatMap (' ':) as ++ ")"

applyRat :: String -> [Rational] -> String
applyRat f rs = applyStr f (map showR rs)

(<=>) :: Rational -> String -> Property
a <=> b = a === evaluate b

-- Instances

alphaFreqList :: [(Int, Gen Char)]
alphaFreqList =
    [ (26, choose ('a', 'z'))
    , (26, choose ('A', 'Z'))]

letter :: Gen Char
letter = frequency alphaFreqList

identifier :: Gen Text
identifier = pack <$> (liftM2 (:) letter $ resize 1 $ sized (`replicateM` letter))

instance Arbitrary Name where
  arbitrary = liftM Name identifier

newtype BExpr = BExpr Expr deriving (Show, Eq)

newtype AExpr = AExpr Expr deriving (Show, Eq)

newtype LExpr = LExpr Expr deriving (Show, Eq)

unAExpr :: AExpr -> Expr
unAExpr (AExpr a) = a

unLExpr :: LExpr -> Expr
unLExpr (LExpr a) = a

aexprTree :: (Ord a, Num a) => a -> Gen AExpr
aexprTree 0 = AExpr <$> liftM  ERat arbitrary
aexprTree n
  | n > 0 = oneof
            [ AExpr <$> liftM  ENeg subtree
            , AExpr <$> liftM2 EAdd subtree subtree
            , AExpr <$> liftM  ERcp subtree
            , AExpr <$> liftM2 EMul subtree subtree
            ]
  where
    subtree = unAExpr <$> aexprTree (n - 1)

varName :: Int -> Gen Name
varName a = Name . pack . (\m -> "x" ++ show m) <$> elements [0..a]

lexprTree' :: Int -> Int -> Int -> Gen LExpr
lexprTree' a _ 0 = LExpr . ERef <$> varName a
lexprTree' a 0 _ = LExpr . ERef <$> varName a
lexprTree' a b n
  | n > 0  = oneof [ LExpr <$> liftM2 ELam v t
                   , LExpr <$> liftM2 EApp t' t'' ]
  where
    v   = varName a
    t   = unLExpr <$> lexprTree' (a + 1) (b + 1) (n - 1)
    t'  = unLExpr <$> lexprTree'  a      (b - 1) (n - 1)
    t'' = unLExpr <$> lexprTree'  a       b      (n - 1)

lexprTree :: Int -> Gen LExpr
lexprTree x = lexprTree' 0 x x

instance Arbitrary AExpr where
  arbitrary = sized aexprTree

-- Count

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

memoize2 :: (Int -> Int -> v) -> (Int -> Int -> v)
memoize2 v = memoize (memoize . v)

memoFix2 :: ((Int -> Int -> v) -> Int -> Int -> v) -> (Int -> Int -> v)
memoFix2 ff = f where f = memoize2 (ff f)

count' :: (Int -> Int -> Integer) -> Int -> Int -> Integer
count' _   0 _ = 0
count' _   1 f = fromIntegral f
count' cnt n f
  | even n     =         theSum endE + cnt (n - 1) (f + 1)
  | otherwise  = extra + theSum endO + cnt (n - 1) (f + 1)
  where
    endE = (n - 2) `div` 2
    endO = (n - 3) `div` 2
    sqr x = x * x
    extra = sqr $ cnt ((n - 1) `div` 2) f
    theSum e = 2 * sigma 1 e (\i -> cnt i f * cnt (n - 1 - i) f)
    sigma a b e = if a < b then e a + sigma (a + 1) b e else e b

count :: Int -> Int -> Integer
count = memoFix2 count'
--count n f = trace ("calling count with: " ++ show n ++ ", " ++ show f) $ memoFix2 count' n f

countLams :: Int -> Int -> Integer
countLams n f = count (n - 1) (f + 1)

countApps :: Int -> Int -> Integer
countApps n f = count n f - countLams n f

genTerm :: Int -> Gen LExpr
genTerm n = gen n 0

gen :: Int -> Int -> Gen LExpr
gen n f
  | n == 1 && f > 0    = LExpr . ERef <$> varName (f - 1)
  | n == 2             = genLamTerm n f
  | otherwise          = genOther n f

genOther :: Int -> Int -> Gen LExpr
genOther n f = do
  r1 <- elements [0 .. count n f]
  r2 <- elements [1 .. (n - 2)]
  r3 <- elements [0 .. countApps n f]
  if r1 < countApps n f
    then genAppTerm n f r2 r3
    else genLamTerm n f

genAppTerm :: Int -> Int -> Int -> Integer -> Gen LExpr
genAppTerm n f i r = trace t $ LExpr <$> liftM2 EApp a1 a2
  where
    t =  ("i: " ++ show i ++ ", c1: " ++ show c1 ++ ", c2: " ++ show c2)
    a1 = unLExpr <$> gen n_1 f
    a2 = unLExpr <$> gen (n - 1 - n_1) f
    sigma a b e = if a < b then e a + sigma (a + 1) b e else e b
    c0 = (count 1 f * count (n - 2) f) - 1
    c1 = sigma 1 (i - 1) (\j -> count j f * count (n - 1 - j) f)
    c2 = sigma 1 i       (\j -> count j f * count (n - 1 - j) f)
--    c2 = c1 + (count i f * count (n - 1 - i) f) - 1
--    i = n - 2                  -- PROBLEM AREA --
    bracket a b1 b2 = (b1 < a) && (a < b2)
    n_1
      | bracket r 0  c0 = 1
      | bracket r c1 c2 = i
--    | otherwise       = i

genLamTerm :: Int -> Int -> Gen LExpr
genLamTerm n f = LExpr . ELam (Name $ pack $ ('x':) $ show f) . unLExpr <$> gen (n - 1) (f + 1)

--genAppTerm

instance Arbitrary LExpr where
  arbitrary = sized genTerm

-- Tests

propEvalFac :: Positive Integer -> Property
propEvalFac (Positive r) = factorial r <=> facStr
  where
    factorial n = toRational $ product [1 .. n]
    facStr      = "(app (mu f (lam x (if (<= x 1) 1 (* x (app f (+ x (- 1))))))) " ++ show r ++ ")"

propEvalRcp :: NonZero Rational -> Property
propEvalRcp (NonZero r) = recip r <=> applyRat "~" [r]

propEvalDiv2 :: Rational -> NonZero Rational -> Property
propEvalDiv2 r1 (NonZero r2) = (r1 / r2) <=> applyStr "*" [showR r1, applyRat "~" [r2]]

propEvalMulN :: NonEmptyList Rational -> Property
propEvalMulN (NonEmpty rs) = product rs  <=> applyRat "*" rs

propEvalAddN :: NonEmptyList Rational -> Property
propEvalAddN (NonEmpty rs) = sum rs      <=> applyRat "+" rs

propEvalMul2 :: Rational -> Rational -> Property
propEvalMul2 r1 r2 = propEvalMulN $ NonEmpty [r1, r2]

propEvalAdd2 :: Rational -> Rational -> Property
propEvalAdd2 r1 r2 = propEvalAddN $ NonEmpty [r1, r2]

-- propEvalStep :: LExpr -> Property
-- propEvalStep e = eval' ce === eval' (step ce) where ce = (empty, e)

propEvalStep :: LExpr -> Property
propEvalStep (LExpr e) = eval' ce === eval' (step ce) where ce = return e

propEvalArith :: AExpr -> Property
propEvalArith (AExpr a) = case eval' $ return a of
                           c@(Clsr _ _ i@(ERat _)) -> c === return i
                           _                       -> property False


propEvalGroup :: [Test]
propEvalGroup =
    [ testProperty "Add n numbers" propEvalAddN
    , testProperty "Add two numbers" propEvalAdd2
    , testProperty "Multiply n numbers" propEvalMulN
    , testProperty "Multiply two numbers" propEvalMul2
    , testProperty "Reciprocal nonzero number" propEvalRcp
    , testProperty "Divide two numbers" propEvalDiv2
    , testProperty "Factorial of a positive integer" propEvalFac
--    , testProperty "Step idempotency on evaluated values" propEvalStep
    ]

tests :: IO [Test]
tests = return [ testGroup "Evaluator tests" propEvalGroup  ]
