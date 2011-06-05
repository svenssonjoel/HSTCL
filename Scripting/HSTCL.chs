{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS  -XDeriveDataTypeable #-}


module Scripting.HSTCL where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable


#include <tcl8.5/tcl.h>


-- 
newtype Interpreter = Interpreter {fromInterpreter :: Ptr ()}
newtype Object      = Object {fromObject :: Ptr ()}

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
{# fun unsafe Tcl_CreateInterp as createInterpreter 
   { } ->   `Interpreter' Interpreter #}


{# fun unsafe Tcl_DeleteInterp as deleteInterpreter
   { fromInterpreter  `Interpreter' } -> `()' #} 

{# fun unsafe Tcl_Eval as evaluate 
   { fromInterpreter `Interpreter' ,
     withCString* `String' } -> `Result' resultToEnum #} 
{#fun unsafe Tcl_EvalFile as evaluateFile 
   { fromInterpreter `Interpreter' , 
     withCString* `String' } -> `Result' resultToEnum #} 

{# fun unsafe Tcl_GetStringResult as getStringResult
   { fromInterpreter `Interpreter' } -> `String' peekCString* #} 

------------------------------------------------------------------------------