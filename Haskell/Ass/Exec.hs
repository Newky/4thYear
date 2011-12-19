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
-- Count as a command takes a list of ident and values.
-- execount counts the number of records for which those ident vals match.
-- Outputs on the handle pointed to by o (stdout by default)
exec Count xs con@((Just m), s, o) = do
		   let str =execcount xs m 
		   hPutStrLn o $ str
		   return con

-- Datefix If its an ident string, we want it as a column number.
-- Convert and pass it back in as a Column Value pair.
exec DateFix ((Ident str):(Value f):[]) con@((Just m), s, o) = do
			case elemIndex str ((headings.head) m) of
				Just a -> exec DateFix ((Col a):(Value f):[]) con
				Nothing -> do
					  putStrLn $ "Error with Ident String"
					  return con
-- Datefix first checks if the column is a date column.
-- I do this by simply regexing the heading for that column.
-- If its includes dates its assumed to have a date field.				
-- It then converts each of the dates on the fly.
exec DateFix ((Col x):(Value f):[]) con@((Just m), s, o)
	| not $ is_valid_column_type "date" heading  = do
							putStrLn "Not a date column"
							return con
	| (is_valid_date_format f) = do
					let newm = map (\r -> map (\c -> ifreplace heading (convert_date_to_format f) c ) r) m
					return ((Just newm), s, o)
	| otherwise = do
			putStrLn "Invalid date format."
			return con
	where heading = ( ( headings. head) m)  !! x

-- Delete takes a row number represented by Col x (just for ease sakes. sorry about the confusion)
-- removes that record and returns the new model :)
exec Delete ((Col x):[]) con@((Just m), s, o) = do
					let newm = removeAt x m
					return ((Just newm), s, o)
-- Again convert string to column no.
exec Distinct ((Ident str):[]) con@((Just m), s, o) = case col_no of
							Just a -> exec Distinct ((Col a):[]) con
							Nothing -> do
									putStrLn "Invalid Column name"
									return con
							where col_no = col_no_from_name (head m) str
-- Distinct firsts gets a certain column identified by x
-- Removes blanks from that column.
-- Then removes duplicates.
exec Distinct ((Col x):[]) con@((Just m), s, o) = do	
				   let dist = r_dup $ rm_blanks $ get_column x m
			           hPutStrLn o $ (show $ length  dist) ++ " Distinct Records"
				   return con
-- List takes a list of Col Value and outputs that list to the 
-- handle pointed to by o
exec List xs con@((Just m), s, o) = do
		   let newm = (head m):execlist xs m
		   hPutStrLn o $ spreadsheet2str newm 
		   return con

--Load up the filename pointed to by X
--This maps a csv file to the internal representation of [[Field]]
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

-- GridFix Takes a column value pair and once it has ensured
-- that that column contains grids, it can do the grid transform.
-- using the function from the Dates.hs for grids.
exec GridFix ((Col x):(Value g):[]) con@((Just m), s, o)
	| not $ is_valid_column_type "grid" heading  = do
							putStrLn "Not a grid column"
							return con
	| (g == "4") || (g == "6") = do
					let newm = map (map (ifreplace heading (convert_grid_to_format (read g :: Int)))) m
					return ((Just newm), s, o)
	| otherwise = do
			putStrLn $ "Grid format has to be a 4 or a 6."
			return con
	where heading = ( ( headings. head) m)  !! x

-- Insert at the moment if given no arguments will 
-- do a interactive readline with the user for each of the heading.
-- when it has got the record. it adds it to the database.
exec Insert [] con@((Just m), s, o) = do
			let heads = headings (head m)
			rec <- record_create heads []
			let newm = (head m):rec:(tail m)
			return ((Just newm), s, o)
					
-- NoOutput sets the output handle back to
-- stdout, it also closes any handle that is currently open.
exec NoOutput [] con@((Just m), s, o) = do
			           case (o==stdout) of
					False -> do
						hClose o
					_ -> putStrLn $ "Output file not set"
				   return ((Just m), s, stdout)

-- Order uses haskells sort by function
-- This allows me to sort on a number of different columns 
exec Order x con@((Just m), s, o) = do
				    let types = sort_types x
				        newm = (head m):(sortBy (sort_multiple types) (tail m))
				    putStrLn $ (++) (show (length newm)) " Records resorted."
				    return ((Just newm), s, o)

-- Quit
-- important to close handle if output is not stdout.
-- returning Nothing as the model to the main will cause the program to exit.
exec Quit _ con@((Just m), s, o) = do
				   case (o==stdout) of
				   	False -> hClose o
					_ -> return ()
				   return (Nothing, s, o)


-- Reformat on column and something like uppercase/lowercase etc.
-- this function is stored in token inst. and is fed into
-- instf which gives back the correct function to ifreplace.
exec Reformat ((Col x):inst:[]) con@((Just m), s, o) = do
							let heads = headings (head m)
							    col_name = (heads !! x)
							    newm = (head m):map (\row -> map (\col -> ifreplace col_name (instf inst) col) row) (tail m)
							return ((Just newm), s, o)

-- Report registrations is a built in function to work with the selected csv file
-- Need to get the first column and remove duplicates and blanks and count the instances of each
exec Report (Registrations:[]) con@((Just m), s, o) = do
						let first = get_column 0 (tail m)
						    wodup = r_dup $ rm_blanks first
 						hPutStrLn o $ unlines $ map (\full@(Map _ x) ->  x ++ "," ++ (show $ count full first)) wodup
						return con

-- Report with competitions uses the date column and  finds the rows where its
-- before now. It ignores any dates which it cannot parse.
exec Report (Competitions:[]) con@((Just m), s, o) = do
						now <- date
						let rows = filter (\r -> isDateBefore now (r !! 7)) m
						hPutStrLn o $ spreadsheet2str rows 
						return con


exec Save ((Filename x):[]) con@((Just m), s, o) = do
				putStrLn $ (++) "Saving " $ show x 
				savesheet x m	
				return con

-- Select all, simply make the selected spreadsheet (second element of config)
-- the entire spreadsheet.
exec Select (All:[]) con@((Just m), s, o) = do
				putStrLn $ "Selected all records." 
				return ((Just m), m, o)

-- Select the part of the list that satisfies the conditions in xs
-- and push that spreadsheet into the selected component of the config.
exec Select xs con@((Just m), s, o) = do
				let str = (head m):execlist xs m
				putStrLn $ (show $ (length str) - 1 ) ++ " Records Selected."
				return ((Just m), str, o)

-- Set and the filename which is the x
-- will try to open that filename, 
-- and pass the handle up to the config file.
exec Set ((Output x):[]) con@((Just m), s, o) = do
				   hand <- try (openFile x WriteMode)
				   case hand of 
						Left _ ->do 
							   putStrLn "Bad output file."
							   return con 
						Right handle -> do
							   return ((Just m), s, handle)
-- Simply prints the selected element of the spreadsheet
exec Show [] con@((Just m), s, o) = do
				    hPutStrLn o $  spreadsheet2str s
				    return con

-- A row number (again with a Col token)
-- A column name
-- A value to replace.
-- This is tough enuf to implement first it must be executed with relation to
-- the selected component. Then these changes must be merged with the
-- real spreadsheet. See flushChanges.
exec Update args@((Col r):(Ident c):(Value v):[]) con@((Just m), s, o)
				| ((length m) >= r) = do
							let ((Just wm), news, o)  =execupdate args con
							    newm = (flushChanges wm (tail news) 1)
							    full = ((Just newm), news, o)
							return full
				| otherwise = do
						putStrLn $ "Invalid Row Number to Update, Only " ++ (show $ length s) ++ " Records to select from."
						return con
-- ExecCount takes a list of argument tokens and calls exec list, and returns
-- a string which accounts to the length of this.
execcount :: [ArgsToken] -> Spreadsheet -> String
execcount xs m = (++) (show $ length $ selm) " records matching."
		where selm = execlist xs m

execupdate :: [ArgsToken] -> Config -> Config
execupdate ((Col r):(Ident c):(Value v):[]) ((Just m), s, o) = ((Just m), (makechange (show r) c v s), o)

-- Execlist is a function which when given an array of arguments
-- which are col Value pairs, and passes through the spreadsheet
-- with only the rows where that condition is true.
execlist :: [ArgsToken] -> Spreadsheet -> Spreadsheet
execlist _ [] = []
execlist ((Col x):(Value str):xs) m 
	| (x >= (length $ head m)) = []
	| otherwise		 = execlist xs (filterspreadsheet_name col_name str m 1)
	where heads = headings (head m)
	      col_name = (heads !! x)
execlist ((Ident x):(Value str):xs) m = execlist xs (filterspreadsheet_name x str m 1)
execlist [] m =  m

-- Load Sheet trys to read in a csv file, 
-- if the io is successful, it parses this into
-- a spreadsheet format and passes it back.
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

-- Saves a sheet under a certain filename.
savesheet :: String -> Spreadsheet -> IO () 
savesheet x model = do
	handle <- openFile x WriteMode
	hPutStr handle (spreadsheet2str model)
	hClose handle
	putStrLn $ (show $ length model) ++ " records saved."
 	return () 

-- Instf Takes a single token which is input from the reformat function
-- and returns a function :)
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

-- Trims a string
-- strips whitespace, reverses the string.
-- trims for whitespace reverses again
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

-- Count the number of some element el
-- in some row
count ::(Eq a) =>  a -> [a] -> Int
count el row = length $ filter (\x -> x == el) row  

-- This is for glob matching.
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


-- This is for the sort / order by function.
-- first I get the number of argument tokens
-- and I return an array of ints which tell the next
-- function what order the columns are to be sorted in.
sort_types :: [ArgsToken] -> [Int]
sort_types [] = []
sort_types ((Col x):(Ascending):xs) = x:sort_types xs
sort_types ((Col x):(Descending):xs) = x:sort_types xs

-- This takes the int array from sort_types
-- and two lists of the same type.
-- Grabs the xth element of each list and compares them
-- returning an Ordering which is used by SortBy function.
sort_multiple ::(Ord a) => [Int] -> [a] -> [a] -> Ordering
sort_multiple (x:[]) a b = (a !! x) `compare` (b !! x)
sort_multiple (x:xs) a b 
	| ( comp == EQ) = sort_multiple xs a b
	| otherwise = comp
	where comp = (a !! x) `compare` (b !! x)

-- Date simply gets the current time.
-- In a Tuple of Year month day
date :: IO (Integer,Int,Int)
date = getCurrentTime >>= return . toGregorian . utctDay

-- Is Date Before some Field value
-- processes the string in the field value.
-- and compares it.
-- Blanks are assumed to be false.
isDateBefore :: (Integer, Int, Int) -> Field -> Bool
isDateBefore _ (Blank _) = False
isDateBefore now@(y, m, d) (Map x val) = case field_date of
					Just a -> before a (d, m, (fromInteger y)) 
					Nothing -> False
				where field_date = (getdate val)

-- I can prob right this better, but will do.
before :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
before (d, m, y) (d2, m2, y2)
	| (y2 > y) = True
	| (y2 == y) && (m2 > m) = True
	| (y2 == y) && (m2 == m) && (d2 == d) = True
	| (y2 == y) && (m2 == m) = (d2 > d)
	| otherwise = False
