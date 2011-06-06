
module Main where 

import Scripting.HSTCL as TCL 
import System.IO

import Foreign.Ptr
import Control.Monad


apaHandler :: HandlerFunc 
apaHandler _ interp _ _ = do
  putStrLn "APA"
  resetResult (Interpreter interp) -- Fix !
  return 0

main = do 
  TCL.withInterpreter $ \i -> do 
    f <- mkHandler apaHandler
    putStrLn (show (castFunPtrToPtr f))
    p <- createObjectCommand i 
                             "apa" 
                             f
                             nullPtr
                             (castPtrToFunPtr nullPtr)
    putStrLn (show (fromCommand p))
    setVar' i "mjau" "123" NoFlag
    interpreterLoop i
    freeHaskellFunPtr f
    
 
  where 
    interpreterLoop i = do 
      putStr "> " 
      hFlush stdout
      str <- getLine
      if  (str == "exit") 
        then 
           (do putStrLn "Quiting!" 
               return ())
        else  
           (do res <- TCL.evaluate i str
               case res of 
                 Ok -> putStrLn "Ok"
                 Error -> putStrLn "Error"
                 Continue -> putStrLn "Continue"
               interpreterLoop i) 