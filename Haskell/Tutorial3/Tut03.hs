-- Richard Delaney 08479950
module Tut03 where

import System.IO

main :: IO ()
main = do
	calc "example.txt"

calc :: FilePath -> IO ()
calc path = do
	handle <- openFile path ReadMode
	contents <- hGetContents handle
	putStr "Subtotals:\n" 
	let subs = map (\x -> foldr (+) 0 $ str2int $ words x ) $ lines contents
	    total = sum subs
	putStr $ unwords $ map (\num -> show num) subs
	putStr "\nTotals:\n"
	putStrLn $ show total

str2int :: [String] -> [Int]
str2int [] = []
str2int (x:xs) = (read x::Int) : str2int xs

