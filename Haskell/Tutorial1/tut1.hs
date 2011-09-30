lastof :: [a] -> a
lastof (x:[]) = x
lastof (x:xs) = lastof xs

isordered :: [Integer] -> Bool
isordered [] = True
isordered (x:[]) = True
isordered (x:y:xs) = (x<=y) && (isordered xs)

seconds :: [a] -> [a]
seconds [] = []
seconds (x:[]) = [x]
seconds (x:y:xs) = [x] ++ seconds xs

riffle :: [a] -> [a] -> [a]
riffle [] [] = []
riffle (x:xs) (y:ys) = [x, y] ++ riffle xs ys

unixname :: [Char] -> [Char]
unixname [] = []
unixname (x:xs) 
		| vowel x = unixname xs
		| otherwise = [x] ++ unixname xs

censor :: [Char] -> [Char]
censor [] = []
censor (x:xs) 
	      | vowel x   = "x" ++ censor xs
	      | otherwise = [x] ++ censor xs


vowel :: Char -> Bool
vowel x = elem x vowels 
vowels = "aeiou"
