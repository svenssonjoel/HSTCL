
module Main where 

import Scripting.HSTCL as TCL 
import System.IO

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Control.Monad


apaHandler :: HandlerFunc 
apaHandler _ interp _ _ = do
  putStrLn "APA"
  resetResult interp
  return 0
  
withArgs :: HandlerFunc  
withArgs _ interp argc argv = do  
  putStrLn$ "Number of Arguments passed:" ++ show argc
  arg0' <- peekArray (fromIntegral argc) argv
  (result,arg0) <- getIntFromObject interp (arg0' !! 1 )
  (result,arg1) <- getIntFromObject interp (arg0' !! 2 )
  putStrLn$ "arg 0 interpreted as Int:" ++ show arg0 ++ " " ++ show arg1
  resetResult interp
  return 0
  
main = do 
  TCL.withInterpreter $ \i -> do 
    f <- mkHandler withArgs
    putStrLn (show (castFunPtrToPtr f))
    p <- createObjectCommand i 
                             "apa" 
                             f
                             nullPtr
                             (castPtrToFunPtr nullPtr)
    putStrLn (show (fromCommand p))
    o <- newStringObject "123"
    setVarFromObject' i "mjau" o [] 
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