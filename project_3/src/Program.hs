-- Leon Krasniqi and Simon Gustafsson

module Program(T, parse, fromString, toString, exec) where

import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

instance Eq T where
  p1 == p2 = toString p1 == toString p2

instance Show T where
  show = toString

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program stmts) = concatMap Statement.toString stmts

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) input = Statement.execute stmts Dictionary.empty input
