
module Main where


import Scripting.HSTCL as TCL 
import System.IO


main = do 
  TCL.withInterpreter interpreterLoop 
    
  putStrLn "hello"
  where 
    interpreterLoop i = do 
      putStr "> " 
      hFlush stdout
      str <- getLine
      res <- TCL.evaluate i str
      case res of 
        Ok -> putStrLn "Ok"
        Error -> putStrLn "Error"
        Continue -> putStrLn "Continue"
      interpreterLoop i 