-- Leon Krasniqi and Simon Gustafsson

module Statement(T, parse, toString, fromString, execute) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =
    Assignment String Expr.T 
    | Skip 
    | Begin [Statement]
    | While Expr.T Statement
    | Read Expr.T
    | Write Expr.T 
    | If Expr.T Statement Statement
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment

skip = accept "skip" -# require ";" >-> \_ -> Skip

begin = accept "begin" -# iter parse #- require "end" >-> Begin

while = accept "while" -# Expr.parse  #-  require "do" # parse >-> uncurry While

readStmt = accept "read" -# word #- require ";" >-> (Read . Expr.Var)

writeStmt = accept "write" -# Expr.parse #- require ";" >-> Write

ifStmt = accept "if" -#  Expr.parse # (require "then" -# parse) # (require "else" -# parse) >-> uncurry (uncurry If)

class Executable t where
    execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where
    -- execute :: [Statement] -> Dictionary.T String Integer -> [Integer] -> [Integer]
    execute (If cond thenStmts elseStmts: stmts) dict input =
        case (Expr.value cond dict) of
            Left err -> error err
            Right v ->
                if v > 0 then
                    execute (thenStmts: stmts) dict input
                else
                    execute (elseStmts: stmts) dict input


instance Parse Statement where
  parse = assignment ! skip ! begin ! while ! readStmt ! writeStmt ! ifStmt
  toString = error "Statement.toString not implemented"
