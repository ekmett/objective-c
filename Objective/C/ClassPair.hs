{-# LANGUAGE ForeignFunctionInterface #-}
module Objective.C.ClassPair
  ( ClassPair(..)
  ) where

import qualified System.IO.Unsafe as Unsafe
import Foreign.C.String
import Objective.C.Class

foreign import ccall unsafe "objc/runtime.h" objc_getClass :: CString -> IO (Class a)
-- foreign import ccall unsafe "objc/runtime.h" objc_getMetaClass :: CString -> IO (Class a)

class ClassPair a where
  getClassNameByProxy :: p a -> String

  getClass :: Class a
  getClass = r where
    r = Unsafe.unsafePerformIO $ withCString (getClassNameByProxy r) objc_getClass

{-
  getMetaClass :: Class (Class a)
  getMetaClass = r where
    r = Unsafe.unsafePerformIO $ Class <$> withCString (getClassNameByProxy (argOf r)) c_objc_getMetaClass
    argOf :: a -> f a -> a
    argOf a _ = a
-}
