
module Main where 

import Scripting.HSTCL as TCL 
import System.IO

import Foreign.Ptr
import Control.Exception

apaHandler :: HandlerFunc 
apaHandler _ interp _ _ = do
  putStrLn "APA is good!!"
  resetResult (Interpreter interp) -- Fix !
  return 0

main = do 
  TCL.withInterpreter $ \i -> do 
    bracket (mkHandler apaHandler) 
      (\ f -> do 
          putStrLn (show (castFunPtrToPtr f))
          p <- createObjectCommand i 
                                   "apa" 
                                   f
                                   nullPtr
                                   (castPtrToFunPtr nullPtr)
          putStrLn (show (fromCommand p))
          interpreterLoop i)
      (freeHaskellFunPtr)
    
 
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