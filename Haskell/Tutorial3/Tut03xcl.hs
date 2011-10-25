-- Richard Delaney 08479950
module Tut03xcl where

import System.IO

type Stack = [Int]

--Run Exercise 1
main :: IO ()
main = xcl []

-- Run Exercise 2
-- main = do
-- 	calc "example.txt"

calc :: FilePath -> IO ()
calc path = do
	handle <- openFile path ReadMode
	contents <- hGetContents handle
	let subs = map (\x -> foldr (+) 0 $ str2int $ words x ) $ lines contents
	    total = sum subs
	    subsstr = unwords $ map (\num -> show num) subs
	putStrLn $ "Subtotals:\n" ++ subsstr ++ "\nTotal:\n" ++ show total

str2int :: [String] -> [Int]
str2int [] = []
str2int (x:xs) = (read x::Int) : str2int xs

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
	| otherwise = checkop num stack

checkop :: String -> Stack -> IO b
checkop "*" stack = doop (*) stack
checkop "+" stack = doop (+) stack
checkop "-" stack = doop (-) stack
checkop x stack = xcl stack


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
