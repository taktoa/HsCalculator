{-# LANGUAGE TupleSections #-}
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

import           Control.Monad            (unless)
import           Data.Map.Strict          (empty)
import           Data.Text                (pack)
import           Eval
import           Parse
import           PrettyPrint
import           System.Console.Haskeline
import           Text.Parsec              (parse)

testEval :: String -> String
testEval = either show (cpPrint . eval' . (empty,)) . parse exprParse "stdin" . pack

testString :: String
testString = "(app (mu f (lam x (if (<= x 1) 1 (* x (app f (+ x (- 1))))))) 20)"

main :: IO ()
main = runInputT defaultSettings loop >> putStrLn "Goodbye!"
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "==> "
      case minput of
       Nothing -> return ()
       Just input -> unless (input `elem` exits) $ outputStrLn $ testEval input
      unless (maybe False (`elem` exits) minput) loop
    exits = [":q", "quit", "exit", "(quit)"]
