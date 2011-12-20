module Parse where
import Data.Char
import Control.Monad.Instances

-- Each user input will be made up of a CmdToken
-- Plus 0 or more ArgsToken
data CmdToken =  Load | Save | Report | Set  | NoOutput | List | Count | Distinct | DateFix | GridFix | Reformat | Select | Show | Order | Update | Delete | Insert | Help | Quit
	deriving(Eq, Show)
data ArgsToken = Filename String | Registrations | Competitions | Output String | Col Int | Ident String | Value String | UpperCase | LowerCase | Capitalize | Trim | All | Ascending | Descending
	deriving(Eq, Show)

--I Use either for some error handling. This declares the
--monad instance for the Either time
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

cmddict = [("load", Load), ("save", Save), ("report", Report), ("set", Set), ("distinct", Distinct), ("count", Count),("list", List),("nooutput", NoOutput), ("update", Update),
		("date-fix", DateFix), ("grid-fix", GridFix), ("reformat", Reformat), ("select",Select), ("show", Show),("order", Order), ("delete", Delete), ("insert", Insert),
			("help", Help), ("quit", Quit)]


--This is the main function called, takes user parses it 
--Outputs a CmdToken and list of ArgsToken
parse :: String -> Either String (CmdToken, [ArgsToken])
parse xs
	| (args == 0) = Left "No Input"
	| (args == 1) = genparse (head bits) []
	| (args >= 2)  = genparse (head bits) $ tail bits 
	| otherwise  = Left "Ugh"
	where bits = wordsSep ' ' xs
	      args = length bits

-- Gen parse checks that the first space seperated token is a
-- command. If it is, it passes the remaining args to the parseArgs function
genparse :: String -> [String] -> Either String (CmdToken, [ArgsToken])
genparse h xs = case (find (cmddict) h) of
			Just a -> case rest of
				  Left str -> Left str
				  Right x -> Right (a, x)
				  where rest = parseArgs a xs
			Nothing -> Left "Unknown Command. Sorry :("
-- ParseArgs takes some CmdToken and ensures that the arguments
-- are the correct ones.
parseArgs :: CmdToken -> [String]  -> Either String [ArgsToken]
parseArgs Load (xs:[]) = case str of
				Just s -> Right [(Filename s)]
				Nothing -> Left ("Invalid Input after the load. " ++ xs)
			where str = parseQuote xs

parseArgs Save (xs:[]) = case str of
				Just s -> Right [(Filename s)]
				Nothing -> Left ("Invalid Input after the save. " ++ xs)
			where str = parseQuote xs

parseArgs Report ("registrations":[]) = Right [Registrations]
parseArgs Report ("competitions":[]) = Right [Competitions]
parseArgs Report (xs:[]) = Left ("Invalid Input after report. "++ xs )

--I have decided to represent output as a argument.
--but its syntactic sugar doesnt get passed out.
parseArgs Set ("output":xs:[]) = case str of
							Just s -> Right [(Output s)]
							Nothing -> Left "Invalid file identifier construct given to set output"
							where str = parseQuote xs
parseArgs Set _ = Left "Set command must be followed by output and a filename surrounded by quotes."

--Distinct takes a single 
parseArgs Distinct (xs:[]) = fmap (replicate 1) $ parseIdent xs
-- These are all the same except those that call 
-- parseIdentVals rather than a single parseIdentVal
-- deal with multiple ident and value pairs.
parseArgs Count xs = parseIdentVals '=' xs
parseArgs List xs = parseIdentVals '=' xs
parseArgs Insert [] =  Right []
parseArgs Insert xs = parseIdentVals '=' xs
parseArgs Select ("all":[]) = Right [All]
parseArgs Select xs = parseIdentVals '=' xs
-- This is kind of hacky but parseIdentVal is a nice generic function :)
parseArgs DateFix xs = parseIdentVal ' ' $ unwordsSep ' ' xs
parseArgs GridFix (x:y:[])
	| (y == "6") || (y == "4") = case (parseIdent x) of
					Left str -> Left str 
					Right a -> Right (a:[Value y])
	| otherwise = Left "GridFix takes a number as its second argument 4 or 6."

-- Reformat takes some identifier
-- and one of the four keywords. 
parseArgs Reformat (x:y:[])  = case ident of
					Right a -> case y of
							"uppercase" -> Right ( a:[(UpperCase)] )
							"capitalize" -> Right ( a:[(Capitalize)] ) 
							"lowercase" -> Right ( a:[(LowerCase)] )
							"trim" -> Right ( a:[(Trim)] )
							_ -> Left "Invalid use of reformat. reformat <column identifier> (uppercase | capitalize | lowercase | trim)"
					Left str -> Left str
				where ident = parseIdent x 

-- Take a ident and put it in an array
parseArgs Delete (xs:[]) = fmap (replicate 1) (parseIdent xs)

-- Update takes 3 things.
parseArgs Update full@(x:y:z:[]) 
		| isDigitStr x = parseUpdate full
		| otherwise = Left "Incorrect first argument must be a number"
parseArgs Update xs = Left "Incorrect number of arguments to update"

-- Parse the Order command
-- Take in a group of arguments
-- discard the by
parseArgs Order ("by":xs) = case error of
					Right _ -> (\x -> if (odd $ length x) then Left "Incorrect Number of Arguments" else Right x) correct
					Left str -> Left str
					where all =  map (\x -> parseOrder x) $ xs
					      error = foldr (>>) (all !! 0) all 
					      correct = map (\x -> fromRight x) all
parseArgs Help (x:[]) = Right [(Value x)]

-- Base cases :)
parseArgs _ [] =  Right []
parseArgs _ xs =  Left "Invalid arguments given to function" 

parseOrder "ascending" = Right Ascending 
parseOrder "descending" = Right Descending
parseOrder xs = parseIdent xs

parseUpdate (x:y:z:[]) = case (parseIdent y) of
			Right fiden -> case (parseQuote z) of
					Just a -> Right ((Col (read x::Int)):fiden:[(Value a)])
					Nothing -> Left "Incorrect third argument to update. Must be a valid quoted string"
			Left str -> Left str
			

parseIdentVals :: Char -> [String] -> Either String [ArgsToken]
parseIdentVals s xs = case error of
			Left str -> Left str
			Right _ -> Right $ foldr (++) [] $ map (\x -> fromRight (parseIdentVal s x) ) $ xs
			where all =  map (\x -> parseIdentVal s x) $ xs
			      error = foldr (>>) (all !! 0) all 
fromRight (Right a) = a 

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
-- Takes some quoted string
-- such as "this string" and removes quotes
parseQuote :: String -> Maybe String
parseQuote ('"':[]) = Nothing 
parseQuote ('"':xs) = case lastchar of
			'"' -> (Just remainder)
			_ -> Nothing
			where reversed = reverse xs
			      lastchar = head reversed
			      remainder = reverse $ tail reversed
parseQuote _ = Nothing 

-- Words seperator which works with quotes.
-- i.e "this,\"is,quote\" string" -> ["this", "\"is,quoted\"", "string"]
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
