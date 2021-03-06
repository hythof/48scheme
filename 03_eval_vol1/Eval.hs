module Eval where
import AST

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval x = String $ "can't eval " ++ show x

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("%", numericBinop mod),
              ("quotient", numericBinop quot),
              ("reminder", numericBinop rem)]
              
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] ->LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                           then 0
                           else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
