module Db where
import IO
import Parse 
 
-- This file consists of all functions to do
-- with the Spreadsheet/DB
-- I.e traversing it adding columns, taking away columns.
-- These functions are used exclusively by the Exec.hs file.

data Field = Map String String
	    | Blank String
		deriving(Ord,Eq)
-- Each Record is a variable length list of fields
type Record = [Field]
--Each Spreadsheet is a variable length list of records
type Spreadsheet = [Record]

type Config = (Maybe Spreadsheet, Spreadsheet, Handle)


instance Show (Field) where
	show (Map x y) = y
	show (Blank _) = "" 
	
-- Given a string of headings,
-- asks user for each one 
-- If input is "" then create a Blank field 
-- Else create a Map field
-- return the entire thing as a function
record_create :: [String] -> Record -> IO Record 
record_create [] x = return (reverse x)
record_create (x:xs) y = do
			  putStrLn $ x ++ ":"
			  line <- getLine
			  case (line == "") of
				  True -> record_create xs ((Blank x):y)
				  _ -> record_create xs ((Map x line):y)

--Given a record extracts all the headings which are in it.
-- headings :: Record -> [String]
-- headings [] = [""]
-- headings ((Map x y):xs) = x : headings xs
-- headings ((Blank ""):xs) = headings xs
-- headings ((Blank x):xs) = x : headings xs
-- 
headings :: Record -> [String]
headings = map show

-- Given a row number, column name, and a replacement value and some spreadsheet
-- produce a new spreadsheet with the replacement.
makechange :: String -> String -> String -> Spreadsheet -> Spreadsheet
makechange r c v [] = []
makechange r c v (el@(Map "Row No" no:fs):xs)
		| (no == r) = ( (Map "Row No" no) : (map (ifreplace c (\x -> v)) fs) ) : xs
		| otherwise = el : makechange r c v xs
makechange r c v (x:xs) = x: makechange r c v xs

-- Given a spreadsheet make it into a long string :) :)
spreadsheet2str :: Spreadsheet -> String
spreadsheet2str m= (unlines. map (unwordsSep ',' . map show) ) m

--Takes string from file i.e getcontents
--Turns it into a Spreadsheet
--Passes headings along with the array of the \n split string
--into file2Sheet'
file2Sheet :: String -> Spreadsheet
file2Sheet conts =  file2Sheet' heads total
		    where total = lines conts
			  heads = filter ((\x -> x /= "") . wordsSep ',') (head total)
			  
--Zips headings with each column i.e ("Map Name", "Example")
--Uses str2field to turn this into a field. i.e (Map "Map Name" "Example")
file2Sheet' :: [String] -> [String] -> Spreadsheet
file2Sheet' heads rest = map (\row -> map str2field $ zip heads $ wordsSep ',' row) rest

--Taken a tuple of Column Name and Value
--return a Field which consists of Map ColumnName Value
--or Blank ColumnName 
str2field :: (String, String) -> Field
str2field (x,"") = (Blank x)
str2field (x,y) = Map x y 

--This function takes a Column Name
--A function with which to apply to the value at that column
--And a field with which to do it.
--Returns a new field. 
ifreplace :: String -> (String -> String) -> Field -> Field
ifreplace c f orig@(Map x y)
		| (x == c) = (Map x (f y))
		| otherwise = orig
ifreplace c f (Blank x)
		| (c == x) = (Map x (f ""))
		| otherwise = (Blank x)
--When a user selects a certain piece of the spreadsheet, 
--This is brought into a different spreadsheet model.
--Flush Changes syncronishes both of these with a simple traversal
--and replace.
--Returns updated spreadsheet.
flushChanges :: Spreadsheet -> Spreadsheet -> Int -> Spreadsheet
flushChanges m [] _ = m
flushChanges [] x _ = [] 
flushChanges (x:xs) mods@(( (Map "Row No" no) :fs ) : rs) n
		| ((read no::Int) == n) = fs:(flushChanges xs rs (n+1))
		| otherwise = x:(flushChanges xs mods (n+1))
-- Returns an element from a record which matches a column name
get_el :: Record -> String -> Field
get_el [] col_name = (Blank "")
get_el ((Blank _):xs) col_name = get_el xs col_name
get_el ((Map x y):xs) col_name
	| (x == col_name) = (Map x y)
	| otherwise = get_el xs col_name

--Gets a column of a full spreadsheet
--Given a column number
get_column :: Int -> Spreadsheet -> [Field] 
get_column col_no m = map ( \x -> (x !! col_no) ) m
--Given a column name get its number.
col_no_from_name :: Record -> String -> Maybe Int
col_no_from_name r s = col_no_from_name' r s 0
col_no_from_name' [] _ _= Nothing 
col_no_from_name' ((Map x y):xs) str n
	| (x == str) = Just n
	| otherwise = col_no_from_name' xs str (n+1)
col_no_from_name' ((Blank _):xs) str n = col_no_from_name' xs str (n+1)

