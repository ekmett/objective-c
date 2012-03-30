{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Objective.C.Class
  (
  -- * Classes
    Class(..)
  , getInstanceSize
  , getVersion
  , setVersion
  -- * Superclasses
  , Super
  , getSuperclass
  -- * MetaClasses
  , Meta
  , isMetaClass
  ) where

import Control.Applicative
import Data.Data
import Data.Hashable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Objective.C.Prim
import Objective.C.Util

-- | Classes
newtype Class a = Class { cclass :: CClass } deriving (Eq,Ord,Typeable,Data,Nil,Storable)

instance Hashable (Class a) where
  hashWithSalt n (Class p) = n `hashWithSalt` (fromIntegral (ptrToIntPtr p) :: Int)

getInstanceSize :: Class a -> IO CSize
getInstanceSize (Class c) = c_class_getInstanceSize c
foreign import ccall unsafe "objc/runtime.h class_getInstanceSize" c_class_getInstanceSize :: CClass -> IO CSize

getVersion :: Class a -> IO Int
getVersion (Class c) = fromIntegral <$> c_class_getVersion c
foreign import ccall unsafe "objc/runtime.h class_getVersion" c_class_getVersion :: CClass -> IO CInt

setVersion :: Class a -> Int -> IO ()
setVersion (Class c) i = c_class_setVersion c (fromIntegral i)
foreign import ccall unsafe "objc/runtime.h class_setVersion" c_class_setVersion :: CClass -> CInt -> IO ()

type Meta a = Class (Class a)
isMetaClass :: Class a -> Bool
isMetaClass (Class c) = toBool (c_class_isMetaClass c)
foreign import ccall unsafe "objc/runtime.h class_isMetaClass" c_class_isMetaClass :: CClass -> CBool

type family Super a
getSuperclass :: Class a -> IO (Class (Super a))
getSuperclass (Class c) = Class <$> c_class_getSuperclass c
foreign import ccall unsafe "objc/runtime.h class_getSuperclass" c_class_getSuperclass :: CClass -> IO CClass

