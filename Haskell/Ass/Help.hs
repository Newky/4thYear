module Help where

help :: String -> String
help "load" = "Load a csv file into the program. load \"filename.csv\""
help "save" = "Save the spreadsheet to file. save \"filename.csv\""
help "report" = "Run Builtin reports. report (registrations | competitions )"
help "count" = "Count number of records satisfying a condition"
help "list" = "Show records satisfying a condition"
help "distinct" = "Show distinct fields in a column"
help "output" = "Redirect output to some filename. Set Output \"filename\""
help "nooutput" = "Redirect output to stdout"
help "date-fix" = "Fix Date data. date-fix column-number format"
help "grid-fix" = "Fix Grid data. grid-fix column-number grid-format"
help "reformat" = "Reformat some column with some function. reformat column-number (uppercase | lowercase | trim | capitalize)"
help "sort" = "Sort spreadsheet on certain columns"
help "update" = "Update the spreadsheet. update row-number column-name column-value."
help "delete" = "Delete a row. Delete row-number."
help "insert" = "Insert a row. Insert"
help "help" = "Help and a name of a command will show help on that. e.g help load"
help "quit" = "End the program."
help xs = "Sorry that command does not have a help page, or does not exist"
