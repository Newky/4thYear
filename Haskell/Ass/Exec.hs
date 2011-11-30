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
				    putStrLn $ show $ spreadsheet2str m
				    return con

exec Out [(Filename x)] con@((Just m), s, o) = do
				   return ((Just m), s, x)

exec NoOutput [] con@((Just m), s, o) = do
				   return ((Just m), s, "")

exec Quit _ con@((Just m), s, o) = do
				   return (Nothing, s, o)

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


