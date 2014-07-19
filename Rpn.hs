import Data.List (sum, product)

-- meaningful domain aliases
type Stack = [Float]

removeSpaces = words
getResult    = head

updateStack :: Stack -> String -> Stack
updateStack (x:y:rest) "*"    = (x * y):rest
updateStack (x:y:rest) "/"    = (y / x):rest
updateStack (x:y:rest) "+"    = (x + y):rest
updateStack (x:y:rest) "-"    = (y - x):rest
updateStack (x:y:rest) "^"    = (y ** x):rest
updateStack (x:xs)     "ln"   = log x:xs
updateStack s          "sum"  = [sum s] 
updateStack s          "prod" = [prod s] 
updateStack s str             = read str:s

solveRPN = getResult . (foldl updateStack []) . removeSpaces
