module AST where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordslist contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordslist head ++ "." ++ showVal tail ++ ")"

unwordslist :: [LispVal] -> String
unwordslist = unwords . map showVal
