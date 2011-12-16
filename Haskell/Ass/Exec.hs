module Exec(Field, Record, Spreadsheet,Config,exec) where
import IO
import Parse 
import List
import Data.Char
import Data.Time.Clock
import Data.Time.Calendar
import Dates
import Db
import Control.Applicative


--Each parsed user input is passed to this function
--Along with the current program state.
--It executes and returns a new state wrapped in an IO wrapper.
exec :: CmdToken -> [ArgsToken] -> Config -> IO Config 
exec Count xs con@((Just m), s, o) = do
		   let str =execcount xs m 
		   hPutStrLn o $ str
		   return con

exec DateFix ((Col x):(Value f):[]) con@((Just m), s, o)
	| (is_valid_date_format f) = do
					let newm = map (\r -> map (\c -> ifreplace heading (convert_date_to_format f) c ) r) m
					return ((Just newm), s, o)
	| otherwise = do
			putStrLn $ "Invalid date format."
			return con
	where heading = ( ( headings. head) m)  !! x
							
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
		   let newm = (head m):execlist xs m
		   hPutStrLn o $ spreadsheet2str newm 
		   return con
exec Load ((Filename x):[]) con@((Just m), s, o) = do
				putStrLn $ (++) "Opening " $ show x 
				newm <- loadsheetf x m
				putStrLn $ (show $ (length .headings .head) newm) ++ " Headings."
				putStrLn $ (show $ length (tail newm)) ++ " records loaded."
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

exec Quit _ con@((Just m), s, o) = do
				   case (o==stdout) of
				   	False -> hClose o
					_ -> return ()
				   return (Nothing, s, o)


exec Reformat ((Col x):inst:[]) con@((Just m), s, o) = do
							let heads = headings (head m)
							    col_name = (heads !! x)
							    newm = (head m):map (\row -> map (\col -> ifreplace col_name (instf inst) col) row) (tail m)
							return ((Just newm), s, o)
				  
exec Report (Registrations:[]) con@((Just m), s, o) = do
						let first = get_column 0 (tail m)
						    wodup = r_dup $ rm_blanks first
 						hPutStrLn o $ unlines $ map (\full@(Map _ x) ->  x ++ "," ++ (show $ count full first)) wodup
						return con

exec Report (Competitions:[]) con@((Just m), s, o) = do
						now <- date
						let rows = filter (\r -> isDateBefore now (r !! 7)) m
						hPutStrLn o $ spreadsheet2str rows 
						return con

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

execcount :: [ArgsToken] -> Spreadsheet -> String
execcount xs m = (++) (show $ length $ selm) " records matching."
		where selm = execlist xs m

execupdate :: [ArgsToken] -> Config -> Config
execupdate ((Col r):(Ident c):(Value v):[]) ((Just m), s, o) = ((Just m), (makechange (show r) c v s), o)

execlist :: [ArgsToken] -> Spreadsheet -> Spreadsheet
execlist _ [] = []
execlist ((Col x):(Value str):xs) m 
	| (x >= (length $ head m)) = []
	| otherwise		 = execlist xs (filterspreadsheet_name col_name str m 1)
	where heads = headings (head m)
	      col_name = (heads !! x)
execlist ((Ident x):(Value str):xs) m = execlist xs (filterspreadsheet_name x str m 1)
execlist [] m =  m


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

instf :: ArgsToken -> (String -> String)
instf inst
	| (inst == UpperCase) = (\val -> map (\x->toUpper x) val)
	| (inst == LowerCase) = (\val -> map (\x->toLower x) val)
	| (inst == Capitalize) = (\val -> (toUpper $ head val) : map (\x->toLower x) (tail val))
	| (inst == Trim) = trim 
	| otherwise = (\val -> val)

-- Given something like
-- $1="?this*" It will filter the Spreadsheet based on that.
-- Returning only the records which match this.
filterspreadsheet_name :: String -> String -> Spreadsheet -> Int -> Spreadsheet
filterspreadsheet_name col_name val [] _ = []
filterspreadsheet_name col_name val (x:xs) no
	| containsglob val ( show $ get_el x col_name) = ((Map "Row No" (show no)):x):filterspreadsheet_name col_name val xs (no+1)
	| otherwise = filterspreadsheet_name col_name val xs (no+1)


trim :: String -> String
trim [] = []
trim (' ':xs) = trim xs
trim (x:xs) = x: reverse (trim' (reverse xs))

trim' :: String -> String
trim' [] = []
trim' (' ':xs) = trim' xs
trim' full = full

rm_blanks :: Record -> Record
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

containsglob :: String -> String -> Bool
containsglob [] _ = True
containsglob ['*'] [] = True
containsglob _ [] = False
containsglob f@(x:xs) s@(y:ys)
	| (x == y ) = containsglob xs ys
	| (x == '*') = (containsglob xs s) || (containsglob f ys)
	| (x == '?') = (containsglob xs ys) 
	| otherwise = False

removeAt :: Int -> [a] ->  [a]
removeAt n m  =  remove' n m 1
remove' :: Int -> [a] -> Int -> [a]
remove' _ [] _  = []
remove' n (x:xs) curr 
	| (curr == n) = xs
	| otherwise = x:remove' n xs (curr+1)

sort_types :: [ArgsToken] -> [Int]
sort_types [] = []
sort_types ((Col x):(Ascending):xs) = x:sort_types xs
sort_types ((Col x):(Descending):xs) = x:sort_types xs

sort_multiple ::(Ord a) => [Int] -> [a] -> [a] -> Ordering
sort_multiple (x:[]) a b = (a !! x) `compare` (b !! x)
sort_multiple (x:xs) a b 
	| ( comp == EQ) = sort_multiple xs a b
	| otherwise = comp
	where comp = (a !! x) `compare` (b !! x)

accepted = ["%D", "%F"]
year = ["%Y", "%y"]
 
date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

isDateBefore :: (Integer, Int, Int) -> Field -> Bool
isDateBefore _ (Blank _) = False
isDateBefore now@(y, m, d) (Map x val) = case field_date of
					Just a -> before a (d, m, (fromInteger y)) 
					Nothing -> False
				where field_date = (getdate val)

before :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
before (d, m, y) (d2, m2, y2)
	| (y2 > y) = True
	| (y2 == y) && (m2 > m) = True
	| (y2 == y) && (m2 == m) && (d2 == d) = True
	| (y2 == y) && (m2 == m) = (d2 > d)
	| otherwise = False
