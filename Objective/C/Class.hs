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
newtype Class a = Class { cclass :: Ptr ObjcClass } deriving (Eq,Ord,Typeable,Data,Nil,Storable)

instance Hashable (Class a) where
  hashWithSalt n (Class p) = n `hashWithSalt` (fromIntegral (ptrToIntPtr p) :: Int)

foreign import ccall unsafe "objc/runtime.h class_getInstanceSize" getInstanceSize :: Class a -> IO CSize

getVersion :: Class a -> IO Int
getVersion c = fromIntegral <$> class_getVersion c
foreign import ccall unsafe "objc/runtime.h" class_getVersion :: Class a -> IO CInt

setVersion :: Class a -> Int -> IO ()
setVersion c i = class_setVersion c (fromIntegral i)
foreign import ccall unsafe "objc/runtime.h" class_setVersion :: Class a -> CInt -> IO ()

type Meta a = Class (Class a)
isMetaClass :: Class a -> Bool
isMetaClass = toBool . class_isMetaClass
foreign import ccall unsafe "objc/runtime.h" class_isMetaClass :: Class a -> CBool

type family Super a
foreign import ccall unsafe "objc/runtime.h class_getSuperclass" getSuperclass :: Class a -> IO (Class (Super a))
