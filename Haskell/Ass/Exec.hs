module Exec(Field, Record, Spreadsheet,Config,exec) where
import IO
import Parse 
import List
import Data.Char
data Field = Map String String
	    | Blank String
		deriving(Ord,Eq, Show)
-- Each Record is a variable length list of fields
type Record = [Field]
--Each Spreadsheet is a variable length list of records
type Spreadsheet = [Record]

type Config = (Maybe Spreadsheet, Spreadsheet, Handle)

exec :: CmdToken -> [ArgsToken] -> Config -> IO Config 

exec Count xs con@((Just m), s, o) = do
		   let str =execcount xs m 
		   hPutStrLn o $ str
		   return con


exec Delete ((Col x):[]) con@((Just m), s, o) = do
					let newm = removeAt x m
					return ((Just newm), s, o)

exec Distinct ((Ident str):[]) con@((Just m), s, o) = case col_no of
							Just a -> exec Distinct ((Col a):[]) con
							Nothing -> do
									putStrLn "Invalid Column name"
									return con
							where col_no = col_no_from_name (head m) str

exec Distinct ((Col x):[]) con@((Just m), s, o) = do	
				   let dist = r_dup $ rm_blanks $ get_column x m
			           hPutStrLn o $ (show $ length  dist) ++ " Distinct Records"
				   return con

exec List xs con@((Just m), s, o) = do
		   let newm = execlist xs m
		   hPutStrLn o $ spreadsheet2str newm 
		   return con

exec Load ((Filename x):[]) con@((Just m), s, o) = do
				putStrLn $ (++) "Opening " $ show x 
				newm <- loadsheetf x m
				putStrLn $ (show $ length newm) ++ " records loaded."
				return ((Just newm), s, o)

exec Help [] con = do
		   putStrLn $ "put general help here."
		   return con

exec Help ((Value x):[]) con = do
				putStrLn $ (++) "help for " $ show x
				return con

exec Insert [] con@((Just m), s, o) = do
			let heads = headings (head m)
			rec <- record_create heads []
			let newm = (head m):rec:(tail m)
			return ((Just newm), s, o)
					

exec NoOutput [] con@((Just m), s, o) = do
			           case (o==stdout) of
					False -> do
						hClose o
					_ -> putStrLn $ "Output file not set"
				   return ((Just m), s, stdout)


exec Order x con@((Just m), s, o) = do
				    let types = sort_types x
				        newm = (head m):(sortBy (sort_multiple types) (tail m))
				    putStrLn $ (++) (show (length newm)) " Records resorted."
				    return ((Just newm), s, o)

exec Reformat ((Col x):inst:[]) con@((Just m), s, o) = do
							let heads = headings (head m)
							    col_name = (heads !! x)
							    newm = (head m):map (\row -> map (\col -> ifreplace col_name (instf inst) col) row) (tail m)
							putStrLn $ show (take 5 newm)
							putStrLn $ col_name 
							return ((Just newm), s, o)
				  
exec Report (Registrations:[]) con@((Just m), s, o) = do
						let first = get_column 0 (tail m)
						    wodup = r_dup $ rm_blanks first
 						hPutStrLn o $ unlines $ map (\full@(Map _ x) ->  x ++ "," ++ (show $ count full first)) wodup
						return con


exec Quit _ con@((Just m), s, o) = do
				   case (o==stdout) of
				   	False -> hClose o
					_ -> return ()
				   return (Nothing, s, o)

exec Save ((Filename x):[]) con@((Just m), s, o) = do
				putStrLn $ (++) "Saving " $ show x 
				savesheet x m	
				return con

exec Select (All:[]) con@((Just m), s, o) = do
				putStrLn $ "Selected all records." 
				return ((Just m), m, o)

exec Select xs con@((Just m), s, o) = do
				let str = (head m):execlist xs m
				putStrLn $ (show $ (length str) - 1 ) ++ " Records Selected."
				return ((Just m), str, o)


exec Set ((Output x):[]) con@((Just m), s, o) = do
				   hand <- try (openFile x WriteMode)
				   case hand of 
						Left _ ->do 
							   putStrLn "Bad output file."
							   return con 
						Right handle -> do
							   return ((Just m), s, handle)

exec Show [] con@((Just m), s, o) = do
				    hPutStrLn o $  spreadsheet2str s
				    return con

exec Update args@((Col r):(Ident c):(Value v):[]) con@((Just m), s, o)
				| ((length m) >= r) = do
							let ((Just wm), news, o)  =execupdate args con
							    newm = (flushChanges wm (tail news) 1)
							    full = ((Just newm), news, o)
							return full
				| otherwise = do
						putStrLn $ "Invalid Row Number to Update, Only " ++ (show $ length s) ++ " Records to select from."
						return con

instf inst
	| (inst == UpperCase) = (\val -> map (\x->toUpper x) val)
	| (inst == LowerCase) = (\val -> map (\x->toLower x) val)
	| (inst == Capitalize) = (\val -> (toUpper $ head val) : map (\x->toLower x) (tail val))
	| (inst == Trim) = trim 
	| otherwise = (\val -> val)

record_create :: [String] -> Record -> IO Record 
record_create [] x = return (reverse x)
record_create (x:xs) y = do
			  putStrLn $ x ++ ":"
			  line <- getLine
			  case (line == "") of
				  True -> record_create xs ((Blank x):y)
				  _ -> record_create xs ((Map x line):y)
				  

trim :: String -> String
trim [] = []
trim (' ':xs) = trim xs
trim (x:xs) = x: reverse (trim' (reverse xs))

trim' :: String -> String
trim' [] = []
trim' (' ':xs) = trim' xs
trim' full = full

execupdate :: [ArgsToken] -> Config -> Config
execupdate ((Col r):(Ident c):(Value v):[]) ((Just m), s, o) = ((Just m), (makechange (show r) c v s), o)

headings :: Record -> [String]
headings [] = [""]
headings ((Map x y):xs) = x : headings xs
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
		| (no == r) = ( (Map "Row No" no) : (map (ifreplace c (\x -> v)) fs) ) : xs
		| otherwise = el : makechange r c v xs
makechange r c v (x:xs) = x: makechange r c v xs

ifreplace :: String -> (String -> String) -> Field -> Field
ifreplace c f orig@(Map x y)
		| (x == c) = (Map x (f y))
		| otherwise = orig
ifreplace c f (Blank x)
		| (c == x) = (Map x (f ""))
		| otherwise = (Blank x)

flushChanges :: Spreadsheet -> Spreadsheet -> Int -> Spreadsheet
flushChanges m [] _ = m
flushChanges [] x _ = [] 
flushChanges (x:xs) mods@(( (Map "Row No" no) :fs ) : rs) n
		| ((read no::Int) == n) = fs:(flushChanges xs rs (n+1))
		| otherwise = x:(flushChanges xs mods (n+1))

removeAt :: Int -> [a] ->  [a]
removeAt n m  =  remove' n m 1

remove' :: Int -> [a] -> Int -> [a]
remove' _ [] _  = []
remove' n (x:xs) curr 
	| (curr == n) = xs
	| otherwise = x:remove' n xs (curr+1)

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

col_no_from_name :: Record -> String -> Maybe Int
col_no_from_name r s = col_no_from_name' r s 0

col_no_from_name' [] _ _= Nothing 
col_no_from_name' ((Map x y):xs) str n
	| (x == str) = Just n
	| otherwise = col_no_from_name' xs str (n+1)
col_no_from_name' ((Blank _):xs) str n = col_no_from_name' xs str (n+1)

rm_blanks [] = []
rm_blanks ((Blank _):xs) = rm_blanks xs
rm_blanks (x:xs) = x:rm_blanks xs

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

sort_types :: [ArgsToken] -> [Int]
sort_types [] = []
sort_types ((Col x):(Ascending):xs) = x:sort_types xs
sort_types ((Col x):(Descending):xs) = x:sort_types xs

sort_multiple (x:[]) a b = (a !! x) `compare` (b !! x)
sort_multiple (x:xs) a b 
	| ( comp == EQ) = sort_multiple xs a b
	| otherwise = comp
	where comp = (a !! x) `compare` (b !! x)
