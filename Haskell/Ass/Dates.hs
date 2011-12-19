module Dates where
import Text.Regex.Posix
import Data.Char
import Data.List
import Data.Time.Format
import Data.Time.Calendar
import Db 
import System.Locale (defaultTimeLocale)

type Date = (Int, Int, Int)

-- Some regular expressions for the Date match.
months = ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"]
digityear = "([0-9]{2,4})"
daynum = "^([0-9]{1,2})[-/]"
seasons = "(spring|summer|autumn|winter|Spring|Summer|Autumn|Winter)"
monthname = "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)"
monthnum = "[0-9]{1,2}[-/]"
yoy = "[0-9]{4}/([0-9]{2})"

-- Given a season string
-- Return a (Day, Month) Tuple
get_season :: String -> (Int, Int)
get_season "spring" = (30, 04)
get_season "summer" = (31, 07)
get_season "autumn" = (31, 10)
get_season "winter" = (31, 01)

-- Main date parse function
-- Note I return it as (Day, Month, Year)
-- Not (Year, Month, Day) Like is taken by most popular haskell
-- formats.
-- lowercases every string first.
getdate :: String -> Maybe (Int, Int, Int) 
getdate str = (cleanup . matchday . map (\c -> toLower c)) str

-- Cleanup Takes a maybe date
-- and can clean up the dates.
cleanup :: Maybe Date -> Maybe Date
cleanup Nothing = Nothing
cleanup full@(Just (0,0,0)) = full
cleanup ( Just (x, y, z) )
	| (z < 20) = Just (x, y, (z + 2000))
	| (z < 100) = Just (x, y, (z + 1900))
	| otherwise = Just (x, y, z)

--Matchday matches the day part of each string.
--It uses regex on the daynum string and checks
--whether a day element was found
--If not it sets it to 0 
matchday :: String -> Maybe Date
matchday "completed" = Just (0, 0, 0) 
matchday str = case day_tuple of
			("", d, xs) -> matchmonth (read ((reverse . tail . reverse) d) :: Int) xs
			(x, "", "") -> case season_tuple of
						("", x, y) -> (\(d, m) -> matchyear d m y) $ (get_season (map (\c-> toLower c) x))
					 	(x, "", "") -> matchmonth 0 x
					where season_tuple = (x =~ seasons :: (String, String, String))
		where day_tuple = (str =~ daynum :: (String, String, String))
--Match month does the same as match day
--except there are two seperate regexs to match against the month part.
matchmonth :: Int -> String -> Maybe Date
matchmonth d xs = case month_tuple of
			("", x, y) -> matchyear d (get_month_number x) y
			_ -> case month_tuple2 of
				("", m, y) -> matchyear d (read ((reverse . tail . reverse) m) :: Int) y
				_ -> matchyear d 0 xs 
			      where month_tuple2 = (xs =~ monthnum :: (String, String, String))
		   where month_tuple = (xs =~ monthname :: (String, String, String))
			 month_tuple2 = (xs =~ monthnum :: (String, String, String))

--Match year is the same except it Then has to return the final Maybe Date
matchyear :: Int -> Int -> String -> Maybe Date
matchyear d m "" = Just (d, m, 0)
matchyear d m ('-':xs) = matchyear d m xs
matchyear d m (' ':xs) = matchyear d m xs
matchyear d m xs = case year_tuple of
			("", y, _) -> Just (d, m, (read y:: Int))
			_ -> case yony of
				[[ _, y]] -> Just (d, m, (read y::Int)) 
				_ -> Just (d, m, 0)
			      where yony = (xs =~ yoy :: [[String]])
		    where year_tuple = (xs =~ digityear :: (String, String, String))

get_index ::String -> [String]-> Int -> Maybe Int
get_index el [] n = Nothing 
get_index el (x:xs) n
	| (x =~ el :: Bool) = Just n
	| otherwise = get_index el xs (n+1)

get_month_number mon = case exists of
			Just a -> a
			Nothing -> 0
			where exists = get_index mon months 1


--This function is pivotal to the grid fix function
--The general algorithm is to take only the desired amount of ints.
--It also deals with any trailing letters by pushing the first letter on
--It then sorts the string to push the letter to the front. It then appends the 
--unsorted list of numbers less the number onto the head of the sorted list(i.e the number)
convert_grid_to_format :: Int -> String -> String
convert_grid_to_format _ ""  =  ""
convert_grid_to_format g s
	| (elem letter ['A'..'Z']) = letter : (filter (not . flip elem ['A'..'Z']) rest)
	| otherwise = rest
	where rest =convert_grid_to_format' s g
	      letter = (head . reverse . sort) rest

convert_grid_to_format' full@(x:xs) 0 = []
convert_grid_to_format' [] _ = []
convert_grid_to_format' full@(x:xs) g
	| (elem x ['A'..'Z']) = x:convert_grid_to_format'' xs g
	| isDigit x= x:convert_grid_to_format' xs (g-1)
	| otherwise = convert_grid_to_format' xs g

convert_grid_to_format'' :: String -> Int -> String
convert_grid_to_format'' _ 0 = []
convert_grid_to_format'' [] _ = [] 
convert_grid_to_format'' (x:xs) g
	| isDigit x = x:convert_grid_to_format'' xs (g-1)
	| otherwise = convert_grid_to_format'' xs (g-1)

-- Chain of Regex matches to only allow date formats which have day month and year information
is_valid_date_format s = (s =~ "^%D$" :: Bool) || (s =~ "^%F$" :: Bool)  || ( ((s =~ "%Y" :: Bool) || (s =~ "%y" :: Bool)) 
				&& ((s =~ "%m" :: Bool) || (s =~ "%e" :: Bool))
				&& (s =~ "%d" :: Bool))

-- Function takes a date as a string as in the csv.
-- Formats it into a formatted string using the date format given
convert_date_to_format :: String -> String -> String 
convert_date_to_format f "" = ""
convert_date_to_format f val = case date of
					Just (0, 0, 0) -> val
					Just (d, m, y) -> formatTime defaultTimeLocale f $ fromGregorian (fromIntegral y::Integer) m d
					Nothing -> val 
					where date = getdate val 

is_valid_column_type :: String -> String -> Bool
is_valid_column_type t c = ((map toLower c) =~ t :: Bool)
