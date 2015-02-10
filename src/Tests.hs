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

import           Control.Monad                     (liftM, liftM2, liftM3,
                                                    replicateM)
import           Data.Functor                      ((<$>))
import           Data.Map.Strict                   (empty)
import           Data.Ratio                        (denominator, numerator)
import           Data.Text                         (pack)
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

identifier :: Gen String
identifier = liftM2 (:) letter $ resize 1 $ sized (`replicateM` letter)

instance Arbitrary MName where
  arbitrary = liftM MName identifier

newtype BExpr = BExpr Expr deriving (Show, Eq)

newtype AExpr = AExpr Expr deriving (Show, Eq)

unAExpr :: AExpr -> Expr
unAExpr (AExpr a) = a

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

lexprTree :: (Ord a, Num a) => [(MName, Bool)] -> a -> Gen Expr
lexprTree a 0 = ERef <$> elements (map fst $ filter snd a)
lexprTree a n
  | n > 0  = oneof [ liftM2 (uncurry ELam) vt
                   , liftM2 (uncurry EMu) vt
                   , liftM2 EApp t t ]
  where
    v = (\(j, k) -> (j, not k)) <$> elements a
    t = lexprTree a (n - 1)

instance Arbitrary AExpr where
  arbitrary = sized aexprTree

instance Arbitrary Expr where
  arbitrary = sized $ lexprTree (map (\x -> (MName x, False)) ["a", "b", "c"])

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

propEvalStep :: Expr -> Property
propEvalStep e = eval' ce === eval' (step ce) where ce = (empty, e)

propEvalArith :: AExpr -> Property
propEvalArith (AExpr e) = case eval' (empty, e) of
                           (c, ERat _) -> c === empty
                           _           -> property False


propEvalGroup :: [Test]
propEvalGroup =
    [ testProperty "Add n numbers" propEvalAddN
    , testProperty "Add two numbers" propEvalAdd2
    , testProperty "Multiply n numbers" propEvalMulN
    , testProperty "Multiply two numbers" propEvalMul2
    , testProperty "Reciprocal nonzero number" propEvalRcp
    , testProperty "Divide two numbers" propEvalDiv2
    , testProperty "Factorial of a positive integer" propEvalFac
    , testProperty "Step idempotency on evaluated values" propEvalStep
    ]

tests :: IO [Test]
tests = return [ testGroup "Evaluator tests" propEvalGroup  ]
