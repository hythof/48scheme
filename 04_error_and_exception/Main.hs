module Main where
import Text.Parsec
import Control.Monad.Error
import Eval (eval)
import Parse (parseExpr)
import Error (ThrowsError, LispError(SyntaxError))
import AST (LispVal)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ SyntaxError err
    Right val -> return val

run :: String -> String
run input = case readExpr input of
    Left err -> show err
    Right val -> (show $ eval val)
                 ++ "  # " ++ show val

main = do
    putStrLn $ run "(/ (+ 1 2 3) 3)"
    putStrLn $ run "(/ 1)"
    putStrLn $ run "()"
    putStrLn $ run "(1)"
    putStrLn $ run "(+ 2 #t)"
    putStrLn $ run "(! 2 3)"
    putStrLn $ run "(+ 2 3)"
