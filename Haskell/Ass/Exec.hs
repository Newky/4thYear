module Exec(Field, Record, Spreadsheet,Config,exec) where
import IO
import Parse 

data Field = Map String String
	    | Blank
		deriving(Eq, Show)
-- Each Record is a variable length list of fields
type Record = [Field]
--Each Spreadsheet is a variable length list of records
type Spreadsheet = [Record]

type Config = (Maybe Spreadsheet, Spreadsheet, String)

exec :: CmdToken -> [ArgsToken] -> Config -> IO Config 
exec Distinct ((Ident str):[]) con@((Just m), s, o) = do
				   let dist = distinct $ get_column_name str m
			           putStrLn $ (show $ length  dist) ++ " Distinct Records"
				   return con

exec Distinct ((Col x):[]) con@((Just m), s, o) = do	
				   let dist = distinct $ rm_blanks $ get_column x m
			           putStrLn $ (show $ length  dist) ++ " Distinct Records"
				   return con

exec Count xs con@((Just m), s, o) = do
		   let str =execcount xs m 
		   putStrLn $ str
		   return con

exec List xs con@((Just m), s, o) = do
		   let newm = execlist xs m
		   putStrLn $ spreadsheet2str newm 
		   return con

exec Load ((Filename x):[]) con@((Just m), s, o) = do
				putStrLn $ (++) "Opening " $ show x 
				newm <- loadsheetf x m
				putStrLn $ (show $ length newm) ++ " records loaded."
				return ((Just newm), s, o)

exec Report (Registrations:[]) con@((Just m), s, o) = do
						let first = get_column 0 m
						    wodup = distinct $ rm_blanks first
						putStrLn $ unlines $ map (\full@(Map _ x) -> show $ x ++ "," ++ (show $ count full first)) wodup					      
						return con


exec Help [] con = do
		   putStrLn $ "put general help here."
		   return con

exec Help ((Value x):[]) con = do
				putStrLn $ (++) "help for " $ show x
				return con

exec Save ((Filename x):[]) con@((Just m), s, o) = do
				putStrLn $ (++) "Saving " $ show x 
				savesheet x m	
				return con

exec Show [] con@((Just m), s, o) = do
				    putStrLn $  spreadsheet2str m
				    return con

exec Out [(Filename x)] con@((Just m), s, o) = do
				   return ((Just m), s, x)

exec NoOutput [] con@((Just m), s, o) = do
				   return ((Just m), s, "")

exec Quit _ con@((Just m), s, o) = do
				   return (Nothing, s, o)

execlist _ [] = []
execlist ((Col x):(Value str):xs) m 
	| (x >= (length $ head m)) = []
	| otherwise		 = execlist xs (filterspreadsheet x str m)
execlist ((Ident x):(Value str):xs) m = execlist xs (filterspreadsheet_name x str m)
execlist [] m =  m

execcount xs m = (++) (show $ length $ selm) " records matching."
		where selm = execlist xs m

loadsheetf :: String -> Spreadsheet -> IO Spreadsheet 
loadsheetf x model = do
	hand <- try (openFile x ReadMode)
	case hand of 
		Left _ ->do 
			   putStrLn "Bad input file."
			   return model
		Right handle -> do
			contents <- hGetContents handle
-- 			putStrLn $ show $ lines contents
			let spread = (file2Sheet contents)
-- 			hClose handle
			return spread

savesheet :: String -> Spreadsheet -> IO () 
savesheet x model = do
	handle <- openFile x WriteMode
	hPutStr handle (spreadsheet2str model)
	hClose handle
	putStrLn $ (show $ length model) ++ " records saved."
 	return () 

spreadsheet2str m = (unlines $ map (\x -> (unwordsSep ',' . map field2str) x) m)

file2Sheet :: String -> Spreadsheet
file2Sheet conts =  file2Sheet' heads rest
		    where total = lines conts
			  heads = wordsSep ',' (head total)
			  rest = tail total

file2Sheet' :: [String] -> [String] -> Spreadsheet
file2Sheet' heads rest = map (\row -> map str2field $ zip heads $ wordsSep ',' row) rest

str2field :: (String, String) -> Field
str2field (x,"") = Blank 
str2field (x,y) = Map x y 

field2str :: Field -> String
field2str (Blank) = ""
field2str (Map x y) = y

distinct :: [Field] -> [Field] 
distinct c = r_dup  c

filterspreadsheet col_no val m = filter (\row -> containsglob val ( get_value row col_no) ) m
filterspreadsheet_name col_name val m = filter (\row -> containsglob val ( field2str $ get_el row col_name) ) m

get_el [] col_name = Blank
get_el ((Blank):xs) col_name = get_el xs col_name
get_el ((Map x y):xs) col_name
	| (x == col_name) = (Map x y)
	| otherwise = get_el xs col_name

get_value :: Record -> Int -> String
get_value rec col_no = field2str (rec !! col_no)

get_column :: Int -> Spreadsheet -> [Field] 
get_column col_no m = map ( \x -> (x !! col_no) ) m

get_column_name col_name m = foldr (++) [] $ map (\row -> filter (isColumn col_name) row   ) m

isColumn col_name (Map x _) = x == col_name
isColumn col_name Blank     = False

rm_blanks [] = []
rm_blanks (x:xs)
	| x == Blank = rm_blanks xs
	| otherwise  = x:rm_blanks xs

r_dup :: (Eq a) => [a] -> [a]
r_dup x = r_dup' x []
r_dup' :: (Eq a) => [a] -> [a] -> [a]
r_dup' [] x'= x'
r_dup' (x:xs) x'
	| elem x x' = r_dup' xs x'
	| otherwise = r_dup' xs (x:x')

count ::(Eq a) =>  a -> [a] -> Int
count el row = length $ filter (\x -> x == el) row  

contains :: String -> String -> Bool
contains _ [] = False 
contains lookup@(x:xs) (y:ys)
		| (x == y) = (contains' xs ys) || contains lookup ys
		| (x /= y) = contains lookup ys

contains' [] _ = True
contains' _ [] = False 
contains' (x:xs) (y:ys)
	| (x == y) = contains' xs ys
	| otherwise = False

containsglob [] _ = True
containsglob ['*'] [] = True
containsglob _ [] = False
containsglob f@(x:xs) s@(y:ys)
	| (x == y ) = containsglob xs ys
	| (x == '*') = (containsglob xs s) || (containsglob f ys)
	| (x == '?') = (containsglob xs ys) 
	| otherwise = False


-- *Cork*
-- Cork
