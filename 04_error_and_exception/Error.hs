module Error where

import Control.Monad.Error
import Text.Parsec
import AST

data LispError = NumArgs Integer [LispVal]
               | TypeMissmatch String LispVal
               | SyntaxError ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message format) = message ++ ": " ++ show format
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs excepted found) = "Excepted " ++ show excepted ++
                                     ", Found " ++ show found
showError (TypeMissmatch excepted found) = "Invalid Type: Excepted " ++ show excepted ++
                                           ", Found" ++ show found
showError (SyntaxError parseError) =     "Parse error at " ++ show parseError

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- expect raise exception when extractValue (Left val). 
