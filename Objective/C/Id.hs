{-# LANGUAGE ForeignFunctionInterface #-}
module Objective.C.Id
  ( Id(..), getClassOf, getClassNameOf
  ) where

import Data.Functor
import Foreign.C.String
import Foreign.Ptr
import Objective.C.Class
import Objective.C.Prim
import Objective.C.Util

foreign import ccall unsafe "objc/runtime.h object_getClass"     c_object_getClass     :: CId -> IO CClass
foreign import ccall unsafe "objc/runtime.h object_getClassName" c_object_getClassName :: CId -> IO CString

-- | Representations of foreign objects
class Nil a => Id a where
  withId :: a -> (Ptr ObjcObject -> IO b) -> IO b

instance Id (Class a) where
  withId (Class cp) f = f (castPtr cp)

getClassOf :: Id a => a -> IO (Class a)
getClassOf a = Class <$> withId a c_object_getClass

getClassNameOf :: Id a => a -> IO String
getClassNameOf a = withId a c_object_getClassName >>= peekCString
