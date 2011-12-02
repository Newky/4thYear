module Exec(Field, Record, Spreadsheet,Config,exec) where
import IO
import Parse 

data Field = Map String String
	    | Blank String
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

exec Select (All:[]) con@((Just m), s, o) = do
				putStrLn $ "Selected all records." 
				return ((Just m), m, o)

exec Select xs con@((Just m), s, o) = do
				let str = (head m):execlist xs m
				putStrLn $ (show $ (length str) - 1 ) ++ " Records Selected."
				return ((Just m), str, o)

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
				    putStrLn $  spreadsheet2str s
				    return con

exec Out [(Filename x)] con@((Just m), s, o) = do
				   return ((Just m), s, x)

exec Update args@((Col r):(Ident c):(Value v):[]) con@((Just m), s, o)
				| ((length m) >= r) = do
							let ((Just wm), news, o)  =execupdate args con
							    newm = (flushChanges wm (tail news) 0)
							    full = ((Just newm), news, o)
-- 							putStrLn $ show $ unlines $ map (\x -> x) (tail news)
							putStrLn $ (show $ length news) ++ " Records."
							return full
				| otherwise = do
						putStrLn $ "Invalid Row Number to Update, Only " ++ (show $ length s) ++ " Records to select from."
						return con
				   


exec NoOutput [] con@((Just m), s, o) = do
				   return ((Just m), s, "")

exec Quit _ con@((Just m), s, o) = do
				   return (Nothing, s, o)

execupdate :: [ArgsToken] -> Config -> Config
execupdate ((Col r):(Ident c):(Value v):[]) ((Just m), s, o) = ((Just m), (makechange (show r) c v s), o)

headings :: Record -> [String]
headings [] = [""]
headings ((Map x y):xs) = (show x) : headings xs
headings ((Blank x):xs) = "" : headings xs

execlist _ [] = []
execlist ((Col x):(Value str):xs) m 
	| (x >= (length $ head m)) = []
	| otherwise		 = execlist xs (filterspreadsheet x str m 1)
execlist ((Ident x):(Value str):xs) m = execlist xs (filterspreadsheet_name x str m 1)
execlist [] m =  m

execcount xs m = (++) (show $ length $ selm) " records matching."
		where selm = execlist xs m

makechange :: String -> String -> String -> Spreadsheet -> Spreadsheet
makechange r c v [] = []
makechange r c v (el@(Map "Row No" no:fs):xs)
		| (no == r) = ( (Map "Row No" no) : (map (ifreplace c v) fs) ) : xs
		| otherwise = el : makechange r c v xs
makechange r c v (x:xs) = x: makechange r c v xs

ifreplace :: String -> String -> Field -> Field
ifreplace c v orig@(Map x y)
		| (x == c) = (Map x v)
		| otherwise = orig
ifreplace c v (Blank x)
		| (c == x) = (Map x v)
		| otherwise = (Blank x)

flushChanges :: Spreadsheet -> Spreadsheet -> Int -> Spreadsheet
flushChanges m [] _ = m
flushChanges [] x _ = [] 
flushChanges (x:xs) mods@(( (Map "Row No" no) :fs ) : rs) n
		| ((read no::Int) == n) = (fs:(flushChanges xs rs (n+1)))
-- 		| (containsglob (show n) no) = fs:(flushChanges xs rs (n+1))
		| otherwise = x:(flushChanges xs mods (n+1))
	-- In the case of bad updates
-- flushChanges (x:xs) (r:rs) n = x: (flushChanges xs rs (n+1))

loadsheetf :: String -> Spreadsheet -> IO Spreadsheet 
loadsheetf x model = do
	hand <- try (openFile x ReadMode)
	case hand of 
		Left _ ->do 
			   putStrLn "Bad input file."
			   return model
		Right handle -> do
			contents <- hGetContents handle
			let spread = (file2Sheet contents)
			return spread

savesheet :: String -> Spreadsheet -> IO () 
savesheet x model = do
	handle <- openFile x WriteMode
	hPutStr handle (spreadsheet2str model)
	hClose handle
	putStrLn $ (show $ length model) ++ " records saved."
 	return () 

spreadsheet2str m = (unwordsSep '\t' (headings $ (head m))) ++ "\n" ++ (unlines $ map (\x -> (unwordsSep ',' . map field2str) x) (tail m))

file2Sheet :: String -> Spreadsheet
file2Sheet conts =  file2Sheet' heads total
		    where total = lines conts
			  heads = wordsSep ',' (head total)

file2Sheet' :: [String] -> [String] -> Spreadsheet
file2Sheet' heads rest = map (\row -> map str2field $ zip heads $ wordsSep ',' row) rest

str2field :: (String, String) -> Field
str2field (x,"") = (Blank x)
str2field (x,y) = Map x y 

field2str :: Field -> String
field2str (Blank _) = ""
field2str (Map x y) = y

distinct :: [Field] -> [Field] 
distinct c = r_dup  c

filterspreadsheet_name col_name val [] _ = []
filterspreadsheet_name col_name val (x:xs) no
	| containsglob val ( field2str $ get_el x col_name) = ((Map "Row No" (show no)):x):filterspreadsheet_name col_name val xs (no+1)
	| otherwise = filterspreadsheet_name col_name val xs (no+1)

filterspreadsheet col_no val [] _ = []
filterspreadsheet col_no val (x:xs) no
	| containsglob val ( get_value x col_no)= ((Map "Row No" (show no)):x):filterspreadsheet col_no val xs (no+1)
	| otherwise = filterspreadsheet col_no val xs (no+1)


get_el [] col_name = (Blank "")
get_el ((Blank _):xs) col_name = get_el xs col_name
get_el ((Map x y):xs) col_name
	| (x == col_name) = (Map x y)
	| otherwise = get_el xs col_name

get_value :: Record -> Int -> String
get_value rec col_no = field2str (rec !! col_no)

get_column :: Int -> Spreadsheet -> [Field] 
get_column col_no m = map ( \x -> (x !! col_no) ) m

get_column_name col_name m = foldr (++) [] $ map (\row -> filter (isColumn col_name) row   ) m

isColumn col_name (Map x _) = x == col_name
isColumn col_name (Blank _)     = False

rm_blanks [] = []
rm_blanks (x:xs)
	| x == (Blank "") = rm_blanks xs
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

containsglob [] _ = True
containsglob ['*'] [] = True
containsglob _ [] = False
containsglob f@(x:xs) s@(y:ys)
	| (x == y ) = containsglob xs ys
	| (x == '*') = (containsglob xs s) || (containsglob f ys)
	| (x == '?') = (containsglob xs ys) 
	| otherwise = False

