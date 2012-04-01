{-# LANGUAGE ForeignFunctionInterface #-}
module Objective.C.Id
  ( Id(..), getClassOf, getClassNameOf
  ) where

import Foreign.C.String
import Foreign.Ptr
import Objective.C.Class
import Objective.C.Prim
import Objective.C.Util

foreign import ccall unsafe "objc/runtime.h" object_getClass     :: CId -> IO (Class a)
foreign import ccall unsafe "objc/runtime.h" object_getClassName :: CId -> IO CString

-- | Representations of foreign objects
class Nil a => Id a where
  withId :: a -> (Ptr ObjcObject -> IO b) -> IO b

instance Id (Class a) where
  withId (Class cp) f = f (castPtr cp)

getClassOf :: Id a => a -> IO (Class a)
getClassOf a = withId a object_getClass

getClassNameOf :: Id a => a -> IO String
getClassNameOf a = withId a object_getClassName >>= peekCString
