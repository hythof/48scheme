module Main where
import Text.Parsec (parse)
import Eval (eval)
import Parse (parseExpr)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val ->
        "expr : " ++ show val ++ "\n" ++
        "value: " ++ (show $ eval val)

main = do
    putStrLn $ readExpr "(/ (+ 1 2 3) 3)"
