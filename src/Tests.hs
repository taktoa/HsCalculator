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

import           Distribution.TestSuite
import           Eval
import           Expr
import           Parse
import           PrettyPrint
import           Test.QuickCheck

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

tests :: IO [Test]
tests = return [ Test succeeds ]
  where
    succeeds = TestInstance
        { run = return $ Finished Pass
        , name = "succeeds"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right succeeds
        }
    -- fails = TestInstance
    --     { run = return $ Finished $ Fail "Always fails!"
    --     , name = "fails"
    --     , tags = []
    --     , options = []
    --     , setOption = \_ _ -> Right fails
    --     }
