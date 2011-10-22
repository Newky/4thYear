-- Richard Delaney 08479950
module Tut03xcl where

main stack = do
	     putStrLn $ show $ stack
	     inst <- getLine
	     let num = head (words inst)
             check num stack

check num stack
	| num == "c" = main []
	| isDigit num = main ( (read num::Int): stack)
	| isOperator num = checkop num stack
	| otherwise = main stack

checkop num stack
	| num == "*" = doop (*) stack
	| num == "+" = doop (+) stack
	| num == "-" = doop (-) stack
	
doop f stack = main ((f op1 op2): stack')
		where stack' = tail $ tail stack
		      op1 = head(stack)
		      op2 = head(tail(stack))

isOperator :: String -> Bool
isOperator x = elem x ["*","+"] 

isDigit :: [Char] -> Bool
isDigit (x:[]) = x >= '0' && x <= '9'
isDigit (x:xs)
	      | x >= '0' && x <= '9' = isDigit xs
	      | otherwise = False
