{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Objective.C.Object
  ( Object(..), Class_, Meta_
  , setClass
  , getClassByName
  , getMetaClassByName
  , copyObject
  ) where

import Data.Data
import Data.Hashable
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe as Unsafe
import Foreign.Ptr
import Objective.C.Prim
import Objective.C.Class
import Objective.C.Id
import Objective.C.Util

-- Objects with unknown classes
newtype Object = Object (ForeignPtr ObjcObject) deriving (Eq,Ord,Show,Data,Typeable,Nil)

instance Hashable Object where
  hashWithSalt n (Object fp) = hashWithSalt n (fromIntegral (ptrToIntPtr (Unsafe.unsafeForeignPtrToPtr fp)) :: Int)

instance Id Object where
  withId (Object fp) = withForeignPtr fp

-- | Unknown Class
type Class_ = Class Object

-- | Unknown MetaClass
type Meta_ = Meta Object

getClassByName :: String -> IO Class_
getClassByName n = withCString n objc_getClass

getMetaClassByName :: String -> IO Meta_
getMetaClassByName n = withCString n objc_getMetaClass

setClass :: Object -> Class a -> IO (Class a)
setClass a c = withId a $ \p -> object_setClass p c

copyObject :: Id a => a -> CSize -> IO Object
copyObject a s = withId a $ \p -> do
  p' <- object_copy p s
  fp' <- newForeignPtr_ p'
  return $ Object fp'

foreign import ccall unsafe "objc/runtime.h" objc_getClass     :: CString -> IO Class_
foreign import ccall unsafe "objc/runtime.h" objc_getMetaClass :: CString -> IO Meta_
foreign import ccall unsafe "objc/runtime.h" object_setClass   :: CId -> Class a -> IO (Class a)
foreign import ccall unsafe "objc/runtime.h" object_copy       :: CId -> CSize -> IO CId

{-
getAssociatedObject :: Id a => a -> String -> IO Object
setAssociatedObject :: (Id a, Id b) => a -> String -> IO b
removeAssociatedObjects :: Id a => a -> IO ()
-}
