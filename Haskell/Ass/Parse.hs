module Parse where
import Data.Char

data CmdToken =  Load | Save | Report | Set | Out | NoOutput | List | Count | Distinct | DateFix | GridFix | Reformat | Select | Show | Order | Update | Delete | Insert | Help | Quit
	deriving(Eq, Show)
data ArgsToken = Filename String | Registrations | Competitions | Output String | Col Int | Ident String | Value String | UpperCase | LowerCase | Capitalize | Trim | All | Ascending | Descending
	deriving(Eq, Show)

--I Use either for some error handling. This declares the
--monad instance.
instance Monad (Either a) where
    return = Right
    Right b >>= f = f b
    Left a >>= _ = Left a

type Dict = [(String, CmdToken)]
type Key = String

find :: Dict -> Key -> Maybe CmdToken

find ((k, v):xs) k'
		| (k' == k) = Just v
		| otherwise = find xs k'
find [] _ = Nothing
cmddict = [("load", Load), ("save", Save), ("report", Report), ("set", Set), ("distinct", Distinct), ("count", Count),("list", List),("out", Out),("nooutput", NoOutput), ("update", Update),
		("date-fix", DateFix), ("grid-fix", GridFix), ("reformat", Reformat), ("select",Select), ("show", Show),("order", Order), ("delete", Delete), ("insert", Insert),
			("help", Help), ("quit", Quit)]


parse :: String -> Either String (CmdToken, [ArgsToken])

parse xs
	| (len == 0) = Left "No Input"
	| (len == 1) = genparse (head bits) []
	| (len >= 2)  = genparse (head bits) (unwords $ tail bits) 
	| otherwise  = Left "Ugh"
	where bits = wordsSep ' ' xs
	      len = length bits


genparse :: String -> String -> Either String (CmdToken, [ArgsToken])
genparse h xs = case (find (cmddict) h) of
			Just a -> case rest of
				  Left str -> Left str
				  Right x -> Right (a, x)
				  where rest = parseArgs a xs
			Nothing -> Left "Unknown Command. Sorry :("

parseArgs :: CmdToken -> String  -> Either String [ArgsToken]

parseArgs Load xs = case str of
				Just s -> Right [(Filename s)]
				Nothing -> Left ("Invalid Input after the load. " ++ xs)
			where str = parseQuote xs

parseArgs Save xs = case str of
				Just s -> Right [(Filename s)]
				Nothing -> Left ("Invalid Input after the save. " ++ xs)
			where str = parseQuote xs

parseArgs Out xs = case str of
				Just s -> Right [(Filename s)]
				Nothing -> Left ("Invalid Input after the save. " ++ xs)
			where str = parseQuote xs

parseArgs Report "registrations" = Right [Registrations]
parseArgs Report "competitions" = Right [Competitions]
parseArgs Report xs = Left ("Invalid Input after report. "++ xs )

parseArgs Set ('o':'u':'t':'p':'u':'t':' ':xs) = case str of
							Just s -> Right [(Output s)]
							Nothing -> Left "Invalid file identifier construct given to set output"
							where str = parseQuote xs
parseArgs Set _ = Left "Set command must be followed by output and a filename surrounded by quotes."

parseArgs Distinct xs = case rest of
			Left str -> Left str
			Right x -> Right [x]
			where rest = parseIdent xs 
parseArgs Count xs = parseIdentVals '=' xs
parseArgs List xs = parseIdentVals '=' xs
parseArgs Insert xs = parseIdentVals '=' xs
parseArgs Select "all" = Right [All]
parseArgs Select xs = parseIdentVals '=' xs
parseArgs DateFix xs = parseIdentVal ' ' xs
parseArgs GridFix xs = parseIdentVal ' ' xs
parseArgs Reformat xs
			| (len < 2) = Left "Invalid use of reformat. reformat <column identifier> (uppercase | capitalize | lowercase | trim)"
			| otherwise = parseRef ((head bits) : [(unwords $ tail bits)])
			where bits = wordsSep ' ' xs
			      len = length bits

parseArgs Delete xs = case rest of
			Left str -> Left str
			Right x -> Right [x]
			where rest = parseIdent xs 

parseArgs Update xs
		| (len < 3) = Left "Insufficient Arguments to Update" 
		| (len == 3) = parseUpdate bits
		| otherwise = Left "Too many arguments to update"
		where bits = wordsSep ' ' xs
		      len = length bits

parseArgs Order ('b':'y':' ':xs) = case error of
					Right _ -> (\x -> if (odd $ length x) then Left "Incorrect Number of Arguments" else Right x) correct
					Left str -> Left str
					where all =  map (\x -> parseOrder x) $ words xs
					      error = foldr (>>) (all !! 0) all 
					      correct = map (\x -> fromRight x) all
		
parseArgs _ [] =  Right []
parseArgs _ xs =  Left "Invalid arguments given to function" 

parseOrder "ascending" = Right Ascending 
parseOrder "descending" = Right Descending
parseOrder xs = case rest of
		Left str -> Left str
		Right x -> Right x
		where rest = parseIdent xs

parseUpdate full@(x:xs)
	| isDigitStr x = parseUpdate' full 
	| otherwise = Left "Incorrect first argument to update. Must be a row number."

parseUpdate' (x:y:z:[]) = case rest of
			Right fiden -> case (parseQuote z) of
					Just a -> Right ((Col (read x::Int)):fiden:[(Value a)])
					Nothing -> Left "Incorrect third argument to update. Must be a valid quoted string"
			Left str -> Left str
			where rest = parseIdent y 

parseRef (x:y:[]) = case rest of
                    Left str -> Left str
                    Right a -> case y of
				"uppercase"	-> Right ( (a : [(UpperCase)]))
				"capitalize"	-> Right ( (a : [(Capitalize)]))
				"trim"		-> Right ( (a : [Trim]))
				"lowercase"	-> Right ( (a : [(LowerCase)]))
				_ -> Left "Invalid use of reformat. reformat <column identifier> (uppercase | capitalize | lowercase | trim)"
                    where rest = parseIdent x 

parseIdentVals :: Char -> String -> Either String [ArgsToken]
parseIdentVals s xs = case error of
			Left str -> Left str
			Right _ -> Right $ foldr (++) [] $ map (\x -> fromRight (parseIdentVal s x) ) $ words xs
			where all =  map (\x -> parseIdentVal s x) $ wordsSep ' ' xs
			      error = foldr (>>) (all !! 0) all 
fromRight (Right a) = a 

-- -- Right ( (parseIdent (identval !! 0)) : (parseIdent (identval !! 0)) )
parseIdentVal :: Char -> String -> Either String [ArgsToken]
parseIdentVal s x 
		| (amount ==  2) = parseIdentVal' identval
		| otherwise = Left ("Invalid IdentVal should include a '=' as a seperator ")
		where 	identval = wordsSep s x
			amount = length $ identval

parseIdentVal' identval = case ident of
			   Right x -> case (parseQuote (identval !! 1)) of
					Just y -> Right (x:[Value y])
					Nothing -> Left "Invalid value parameter in expression"
			   Left str -> Left str 
			   where ident = parseIdent (identval !! 0)

parseIdent :: String -> Either String ArgsToken
parseIdent ('$': xs)
		| isDigitStr xs = Right (Col (read xs::Int))
		| otherwise = Left "Invalid use of $. A number must follow $"  

parseIdent full@('"':xs) = case str of
				Just s -> Right (Ident s)
				Nothing -> Left "Invalid Identifier"
			   where str = parseQuote full 
		 
parseQuote :: String -> Maybe String
parseQuote ('"':[]) = Nothing 
parseQuote ('"':xs) = case lastchar of
			'"' -> (Just remainder)
			_ -> Nothing
			where reversed = reverse xs
			      lastchar = head reversed
			      remainder = reverse $ tail reversed
parseQuote _ = Nothing 

wordsSep :: Char ->String -> [String]
wordsSep s [] = [""]
wordsSep s (x:xs) 
	| x == '"' = (['"'] ++ (head next')):tail next'
	| x == s = "": next
	| otherwise = ([x] ++ (head next)) : tail next 
	where next = wordsSep s xs  
	      next' = wordsSep' s xs

wordsSep' s [] = [""]
wordsSep' s (x:xs) 
	| x == '"' = (['"'] ++ (head next')):tail next'
	| otherwise = ([x] ++ (head next)) : tail next 
	where next = wordsSep' s xs  
	      next' = wordsSep s xs

--Join words with a seperator
unwordsSep :: Char->[String]->String
unwordsSep s (x:[]) = x
unwordsSep s (x:xs) = x ++ [s] ++ (unwordsSep s xs)

isDigitStr = ( foldr (&&) True . map (\x -> isDigit x))
