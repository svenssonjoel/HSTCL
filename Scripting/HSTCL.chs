{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS  -XDeriveDataTypeable #-}


module Scripting.HSTCL where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable


#include <tcl8.5/tcl.h>


newtype Interpreter = Interpreter {fromInterpreter :: Ptr ()}
newtype Object      = Object {fromObject :: Ptr ()}
newtype Command     = Command {fromCommand :: Ptr ()}

------------------------------------------------------------------------------
-- Haskell code

withInterpreter f = createInterpreter >>= 
                    \i -> do { f i ; deleteInterpreter i }


#c
enum RESULT_ENUM { 
  OK = TCL_OK,
  ERROR = TCL_ERROR,
  CONTINUE = TCL_CONTINUE,
  };
#endc

resultToEnum = toEnum . fromIntegral

{# enum RESULT_ENUM as Result 
   {underscoreToCase} deriving (Show, Eq) #}


------------------------------------------------------------------------------
-- Bindings
{# fun  Tcl_CreateInterp as createInterpreter 
   { } ->   `Interpreter' Interpreter #}


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
     id `DeleteFuncPtr'} -> `Command' Command #}

type DeleteFuncPtr = FunPtr (Ptr () -> IO ())

{# fun  Tcl_ResetResult as resetResult
   { fromInterpreter `Interpreter' } -> `()' #}

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Attempt to create a callback wrapper. No support for this in C2HS ?


--static int HandleFireWeaponCmd(ClientData client_data,
--Tcl_Interp * interp,
--int objc, Tcl_Obj * CONST objv[])

type HandlerFunc = Ptr () -> Ptr () -> CInt -> Ptr (Ptr ()) -> IO (CInt)
type HandlerFuncPtr = FunPtr HandlerFunc


foreign import ccall safe "wrapper"
   mkHandler :: HandlerFunc -> IO (FunPtr HandlerFunc)
                
