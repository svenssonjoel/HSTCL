{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS  -XDeriveDataTypeable #-}


module Scripting.HSTCL where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import Control.Exception

import Data.Typeable
import Data.Bits 


-- TODO: dont want to hardcode 8.5 in there
#include <tcl.h>



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


mkStringResult ptr = do 
  throwIfNull ptr "mkStringResult"
  peekCString ptr


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
  
enum LINK_FLAG  {
  GLOBAL_ONLY = TCL_GLOBAL_ONLY,
  NAMESPACE_ONLY = TCL_NAMESPACE_ONLY,
  LEAVE_ERR_MSG = TCL_LEAVE_ERR_MSG,
  APPEND_VALUE = TCL_APPEND_VALUE,
  LIST_ELEMENT = TCL_LIST_ELEMENT
  };
  
#endc

resultToEnum = toEnum . fromIntegral



{# enum RESULT_ENUM as Result 
   {underscoreToCase} deriving (Show, Eq) #}


{# enum LINK_TYPE as LinkType
   {underscoreToCase} deriving (Show, Eq) #}

{# enum LINK_FLAG as LinkFlag
   {underscoreToCase} deriving (Show, Eq) #}


linkTypeIn = fromIntegral . fromEnum
-- linkFlagIn = fromIntegral . fromEnum

flagListToInt :: (Enum a, Bits b, Num b )=> [a] -> b
flagListToInt = foldl (.|.) 0 . map (fromIntegral . fromEnum)

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





type HandlerFunc = Ptr () -> Interpreter -> CInt -> Ptr( Ptr ()) -> IO (CInt) 
type HandlerFuncC = Ptr () -> Ptr () -> CInt -> Ptr (Ptr ()) -> IO (CInt)

type HandlerFuncPtr = FunPtr HandlerFuncC

-- TODO: Do for all other parts of the input what is done in the 
-- Interpreter case. (give proper Haskell types)
mkHandler :: HandlerFunc -> IO (FunPtr HandlerFuncC)
mkHandler f = mkHandler' f'  
  where 
    f' ptr interp cint ptrptr = 
      f ptr (Interpreter interp) cint ptrptr 

foreign import ccall safe "wrapper"
   mkHandler' :: HandlerFuncC -> IO (FunPtr HandlerFuncC)
                


------------------------------------------------------------------------------
-- Low level operations that must be wrapped in something to 
-- be useful for the Haskeller. 


-- TODO: linkVar is a way to communicate values between TCL and C.
--       Figure  something Haskellish out for this purpose.
{# fun Tcl_LinkVar as linkVar' 
   { fromInterpreter `Interpreter' , 
     withCString* `String' , 
     id `Ptr CChar' ,
     linkTypeIn `LinkType' } -> `Result' resultToEnum #}
  

-- TODO: The flag could be a combination of "flags" make this work. somehow
-- TODO: Users of this one may set the second "String" to Null. not possible now. 
{# fun Tcl_SetVar2Ex as setVar2Ex' 
   { fromInterpreter `Interpreter' , 
     withCString*   `String' ,
     withCString*   `String' , 
     fromObject     `Object' ,
     flagListToInt  `[LinkFlag]' } -> `Command' mkCommand* #}
     

{# fun Tcl_SetVar2Ex as setVarFromObject' 
   { fromInterpreter `Interpreter' ,
     withCString*  `String' ,
     withNullPtr-  `String' , 
     fromObject    `Object' , 
     flagListToInt `[LinkFlag]' } -> `Command' mkCommand* #}

withNullPtr f = f nullPtr


{# fun Tcl_SetVar as setVar' 
   { fromInterpreter `Interpreter' ,
     withCString*    `String' , 
     withCString*    `String' ,
     flagListToInt   `[LinkFlag]' } -> `String' mkStringResult* #}
     



------------------------------------------------------------------------------
-- Create Object 

{# fun Tcl_NewObj as newObject
   { } -> `Object' mkObject* #} 

{# fun Tcl_DuplicateObj as duplicateObject
   { fromObject `Object' } -> `Object' mkObject* #} 


-- TODO: IS A MACRO. needs a wrapper
--{# fun Tcl_IncrRefCount as incrRefCount 
--   { fromObject `Object' } -> `()' id #} 


newStringObject str = newStringObject' str (length str)

{# fun Tcl_NewStringObj as newStringObject'
   { withCString* `String' , 
     fromIntegral `Int'    } -> `Object' mkObject* #}
