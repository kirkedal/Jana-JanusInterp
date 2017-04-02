module Jana.ErrorMessages where

import Text.Printf
import Data.List (intercalate)

import Jana.Error
import Jana.Ast


aliasError :: Ident -> Ident -> Message
aliasError id1 id2 = Message $
  printf "Identifiers `%s' and `%s' are aliases" (ident id1) (ident id2)

unboundVar :: String -> Message
unboundVar name = Message $
  printf "Variable `%s' has not been declared" name

alreadyBound :: String -> Message
alreadyBound name = Message $
  printf "Variable name `%s' is already bound" name

typeError :: String -> Message
typeError = Message

typeMismatch :: [String] -> String -> Message
typeMismatch expTypes actualType = Message $
  printf "Couldn't match expected type %s\n\
         \            with actual type `%s'" (joinE expTypes) actualType
  where
    joinE []     = ""
    joinE [x]    = quote x
    joinE [x, y] = quote x ++ " or " ++ quote y
    joinE (x:xs) = quote x ++ ", " ++ joinE xs
    quote s = "`" ++ s ++ "'"

swapTypeError :: String -> String -> Message
swapTypeError typ1 typ2 = Message $
  printf "Can't swap variables of type `%s' and `%s'" typ1 typ2

outOfBounds :: (PrintfArg a) => [a] -> [a] -> Message
outOfBounds index size = Message $
  printf "Array index `%s' was out of bounds (array size was %s)"
         (pa index) (pa size)
  where 
    pa i = "[" ++ intercalate "," (map (printf "%d") i) ++ "]"

emptyStack :: Message
emptyStack = Message "Can't pop from empty stack"

popToNonZero :: Ident -> Message
popToNonZero idnt = Message $
  printf "Can't pop to non-zero variable `%s'" (ident idnt)

assertionFail :: String -> Message
assertionFail s = Message $
  "Assertion failed: " ++ s

delocalNameMismatch :: Ident -> Ident -> Message
delocalNameMismatch id1 id2 = Message $
  printf "Variable names does not match in local declaration:\n\
         \    `%s' in `local'\n\
         \    `%s' in `delocal'\n\
         \`delocal' statements must come in reverse order of the `local' statments"
         (ident id1) (ident id2)

delocalTypeMismatch :: Ident -> String -> String -> Message
delocalTypeMismatch idnt locType delocType = Message $
  printf "Type of variable `%s' does not match local declaration:\n\
         \    `%s' in `local'\n\
         \    `%s' in `delocal'"
         (ident idnt) locType delocType

wrongDelocalValue :: Ident -> String -> String -> Message
wrongDelocalValue idnt expect actual = Message $
  printf "Expected value to be `%s' for local variable `%s'\n\
         \ but actual value is `%s'"
         expect (ident idnt) actual

undefProc :: String -> Message
undefProc name = Message $
  printf "Procedure `%s' is not defined" name

procDefined :: (Identifiable a) => a -> Message
procDefined idnt = Message $
  printf "Procedure `%s' is already defined" (ident idnt)

callingMainError :: Message
callingMainError = Message "It is not allowed to call the `main' procedure"

argumentError :: (Identifiable a, PrintfArg b) => a -> b -> b -> Message
argumentError idnt expect actual = Message $
  printf "Procedure `%s' expects %d argument(s) but got %d"
         (ident idnt) expect actual

arraySize :: Message
arraySize = Message $ "Array size must be greater than or equal to one"

arraySizeMissing :: Ident -> Message
arraySizeMissing idnt = Message $
  printf "Array size missing for variable `%s'" (ident idnt)

arraySizeMismatch :: (PrintfArg a, PrintfArg b) => a -> b -> Message
arraySizeMismatch expr actual = Message $
  printf "Expecting array of size %d\n\
         \           but got size %d"
         expr actual

divisionByZero :: Message
divisionByZero = Message "Division by zero"

noMainProc :: Message
noMainProc = Message "No main procedure has been defined"

multipleMainProcs :: Message
multipleMainProcs = Message "Multiple main procedures has been defined"

procDuplicateArgs :: Proc -> Message
procDuplicateArgs idnt = Message $
  printf "Procedure `%s' has duplicate arguments" (ident idnt)

userError :: String -> Message
userError msg = Message $ "User error: " ++ msg

printfTypeMismatch :: Char -> String -> String -> Message
printfTypeMismatch char expected given = Message $
  printf "Type mismatch for `%%%c' format specifier\n\
         \Expected argument of type `%s'\n\
         \      but actual type was `%s'" char expected given

printfTooManyArgs :: Message
printfTooManyArgs = Message $
  "Not all arguments where used during string formatting"

printfNotEnoughArgs :: Message
printfNotEnoughArgs = Message $
  "Not enough arguments for format string"

printfUnrecognizedType :: Char -> Message
printfUnrecognizedType char = Message $
  printf "Unrecognized format specifier: `%%%c'" char

noExternalCalls :: Message
noExternalCalls = Message $
  printf "Call to external functions is not allowed as statements during interpretation"
