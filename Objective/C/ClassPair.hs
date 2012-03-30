{-# LANGUAGE ForeignFunctionInterface #-}
module Objective.C.ClassPair
  ( ClassPair(..)
  ) where

import qualified System.IO.Unsafe as Unsafe
import Foreign.C.String
import Objective.C.Prim
import Objective.C.Class

foreign import ccall unsafe "objc/runtime.h objc_getClass" c_objc_getClass :: CString -> IO CClass
-- foreign import ccall unsafe "objc/runtime.h objc_getMetaClass" c_objc_getMetaClass :: CString -> IO CClass

class ClassPair a where
  getClassNameByProxy :: p a -> String

  getClass :: Class a
  getClass = r where
    r = Unsafe.unsafePerformIO $ do
      c <- withCString (getClassNameByProxy r) c_objc_getClass
      return $ Class c

{-
  getMetaClass :: Class (Class a)
  getMetaClass = r where
    r = Unsafe.unsafePerformIO $ Class <$> withCString (getClassNameByProxy (argOf r)) c_objc_getMetaClass
    argOf :: a -> f a -> a
    argOf a _ = a
-}
