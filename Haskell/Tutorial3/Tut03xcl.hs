-- Richard Delaney 08479950
module Tut03xcl where
type Stack = [Int]

main :: IO ()
main = xcl []

xcl :: Stack -> IO b
xcl stack = do
	     putStrLn $ show $ stack
	     inst <- getLine
	     if null inst
		then xcl stack
		else let num = head (words inst)
		     in check num stack

check :: String -> Stack -> IO b
check num stack
	| num == "c" = xcl []
	| isDigit num = xcl ( (read num::Int): stack)
	| isOperator num = checkop num stack
	| otherwise = xcl stack

checkop :: String -> Stack -> IO b
checkop num stack
	| num == "*" = doop (*) stack
	| num == "+" = doop (+) stack
	| num == "-" = doop (-) stack

doop :: (Int -> Int -> Int) -> Stack -> IO b	
doop f stack 
		| length stack < 2 = xcl stack
		| length stack >= 2 = xcl ((f op1 op2): stack')
				      where stack' = tail $ tail stack
					    op1 = head(stack)
					    op2 = head(tail(stack))

isOperator :: String -> Bool
isOperator x = elem x ["*","+", "-"] 

isDigit :: [Char] -> Bool
isDigit (x:[]) = x >= '0' && x <= '9'
isDigit (x:xs)
	      | x >= '0' && x <= '9' = isDigit xs
	      | otherwise = False
