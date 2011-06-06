{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS  -XDeriveDataTypeable #-}


module Scripting.HSTCL where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import Control.Exception

import Data.Typeable
#include <tcl8.5/tcl.h>



------------------------------------------------------------------------------
-- Error Handling 

data HSTCLException = HSTCLException String
  deriving (Eq, Show, Typeable)


instance Exception HSTCLException 

throwIfNull   ptr msg = 
  if ptr == nullPtr 
  then throwIO (HSTCLException ("NULL POINTER: "  ++ msg)) 
  else return ptr
throwIfError  res msg = 
  if res == Error   
  then throwIO (HSTCLException ("Error: " ++ msg))
  else return ()
------------------------------------------------------------------------------
-- Types 

newtype Interpreter = Interpreter {fromInterpreter :: Ptr ()}
newtype Object      = Object {fromObject :: Ptr ()}
newtype Command     = Command {fromCommand :: Ptr ()}


mkX x msg ptr = do throwIfNull ptr msg
                   return$ x ptr

mkInterpreter = mkX Interpreter "mkInterpreter"  
mkObject      = mkX Object      "mkObject"
mkCommand     = mkX Command     "mkCommand" 


------------------------------------------------------------------------------
-- Haskell code

withInterpreter f = createInterpreter >>= 
                    \i -> do { f i ; deleteInterpreter i }


#c
enum RESULT_ENUM { 
  OK = TCL_OK,
  ERROR = TCL_ERROR,
  CONTINUE = TCL_CONTINUE
  };
  
  
enum LINK_TYPE {  
  LINK_INT = TCL_LINK_INT,
  LINK_DOUBLE = TCL_LINK_DOUBLE,
  LINK_BOOLEAN = TCL_LINK_BOOLEAN,                
  LINK_STRING = TCL_LINK_STRING
  };
#endc

resultToEnum = toEnum . fromIntegral
enumToC   = fromIntegral . fromEnum

{# enum RESULT_ENUM as Result 
   {underscoreToCase} deriving (Show, Eq) #}


{# enum LINK_TYPE as LinkType
   {underscoreToCase} deriving (Show, Eq) #}

------------------------------------------------------------------------------
-- Bindings
{# fun  Tcl_CreateInterp as createInterpreter 
   { } ->   `Interpreter' mkInterpreter* #}


{# fun  Tcl_DeleteInterp as deleteInterpreter
   { fromInterpreter  `Interpreter' } -> `()' #} 

{# fun  Tcl_Eval as evaluate 
   { fromInterpreter `Interpreter' ,
     withCString* `String' } -> `Result' resultToEnum #} 
{#fun Tcl_EvalFile as evaluateFile 
   { fromInterpreter `Interpreter' , 
     withCString* `String' } -> `Result' resultToEnum #} 

{# fun  Tcl_GetStringResult as getStringResult
   { fromInterpreter `Interpreter' } -> `String' peekCString* #} 


{# fun  Tcl_CreateObjCommand as createObjectCommand
   { fromInterpreter `Interpreter' ,
     withCString* `String' ,
     id  `HandlerFuncPtr', 
     id `Ptr ()', -- ClientData 
     id `DeleteFuncPtr'} -> `Command' mkCommand* #}

type DeleteFuncPtr = FunPtr (Ptr () -> IO ())

{# fun  Tcl_ResetResult as resetResult
   { fromInterpreter `Interpreter' } -> `()' #}

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Attempt to create a callback wrapper. No support for this in C2HS ?

type HandlerFunc = Ptr () -> Ptr () -> CInt -> Ptr (Ptr ()) -> IO (CInt)
type HandlerFuncPtr = FunPtr HandlerFunc


foreign import ccall safe "wrapper"
   mkHandler :: HandlerFunc -> IO (FunPtr HandlerFunc)
                


------------------------------------------------------------------------------
-- Low level operations that must be wrapped in something to 
-- be useful for the Haskeller. 


-- TODO: linkVar is a way to communicate values between TCL and C.
--       Figure  something Haskellish out for this purpose.
{# fun Tcl_LinkVar as linkVar' 
   { fromInterpreter `Interpreter' , 
     withCString* `String' , 
     id `Ptr CChar' ,
     enumToC `LinkType' } -> `Result' resultToEnum #}
  
     