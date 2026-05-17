-- Leon Krasniqi and Simon Gustafsson

module Statement(T, parse, toString, fromString, execute) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =
    Assignment String Expr.T | Skip | Begin T | While Expr.T T | Read Expr.T | Write Expr.T |
    If Expr.T Statement Statement
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment

skip = undefined

begin = undefined

while = undefined

read = undefined

write = undefined

--if = undefined

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
    execute (Assignment )

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
