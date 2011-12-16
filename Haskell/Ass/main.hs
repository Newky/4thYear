module Main where
import IO
import Parse (parse, CmdToken, ArgsToken, unwordsSep)
import Exec 
import System.Console.Readline


main = do 
	cmd ((Just []),[],stdout)

cmd :: Config-> IO ()
cmd (Nothing,_,_)= do
	putStrLn "Ended."
cmd con@((Just model),selected,output)= do
	putStrLn $ (show $ length model) ++ " Records."
	maybline <- readline ">>" 
	case maybline of
		Nothing -> do
				evalline (Left "Invalid Input") con
				cmd con 
		Just line -> do 
				let toks = (parse line)
				newm <- evalline toks con 
				cmd newm

evalline :: Either String (CmdToken, [ArgsToken]) -> Config -> IO Config 
evalline (Right (cmd, args)) c = do
				nm <- exec cmd args c
				return nm 
evalline (Left x) c = do
			putStrLn $ x 
			return c
 
