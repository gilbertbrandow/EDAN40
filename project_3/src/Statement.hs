-- Leon Krasniqi and Simon Gustafsson

module Statement (T, parse, toString, fromString, execute) where

import qualified Dictionary
import qualified Expr
import Parser hiding (T)
import Prelude hiding (fail, return)

type T = Statement

data Statement
  = Assignment String Expr.T
  | Skip
  | Block [Statement]
  | While Expr.T Statement
  | Read String
  | Write Expr.T
  | If Expr.T Statement Statement
  deriving (Show)

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment

skip = accept "skip" -# require ";" >-> \_ -> Skip

block = accept "begin" -# iter parse #- require "end" >-> Block

while = accept "while" -# Expr.parse #- require "do" # parse >-> uncurry While

readStmt = accept "read" -# word #- require ";" >-> Read

writeStmt = accept "write" -# Expr.parse #- require ";" >-> Write

ifStmt = accept "if" -# Expr.parse # (require "then" -# parse) # (require "else" -# parse) >-> uncurry (uncurry If)

class Executable t where
  execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where
  execute [] _ _ = []
  execute (Assignment name expr : stmts) dict input =
    case Expr.value expr dict of
      Left err -> error err
      Right v -> execute stmts (Dictionary.insert (name, v) dict) input
  execute (Skip : stmts) dict input = execute stmts dict input
  execute (Block block : stmts) dict input =
    execute (block ++ stmts) dict input
  execute (While cond doStmt : stmts) dict input =
    case Expr.value cond dict of
      Left err -> error err
      Right v ->
        if v > 0
          then
            execute (doStmt : While cond doStmt : stmts) dict input
          else
            execute stmts dict input
  execute (Read name : stmts) dict (inputVal : restInput) =
    let newDict = Dictionary.insert (name, inputVal) dict
     in execute stmts newDict restInput
  execute (Write expr : stmts) dict input =
    case Expr.value expr dict of
      Left err -> error err
      Right v -> v : execute stmts dict input
  execute (If cond thenStmts elseStmts : stmts) dict input =
    case (Expr.value cond dict) of
      Left err -> error err
      Right v ->
        if v > 0
          then
            execute (thenStmts : stmts) dict input
          else
            execute (elseStmts : stmts) dict input

instance Parse Statement where
  parse = assignment ! skip ! block ! while ! readStmt ! writeStmt ! ifStmt
  toString (Assignment var expr) = var ++ " := " ++ Expr.toString expr ++ ";"
  toString Skip = "skip;"
  toString (Block stmts) = "begin " ++ concatMap toString stmts ++ "end"
  toString (While cond doStmt) = "while " ++ Expr.toString cond ++ " do " ++ toString doStmt
  toString (Read var) = "read " ++ var ++ ";"
  toString (Write expr) = "write " ++ Expr.toString expr ++ ";"
  toString (If cond thenStmt elseStmt) = "if " ++ Expr.toString cond ++ " then " ++ toString thenStmt ++ " else " ++ toString elseStmt

  
