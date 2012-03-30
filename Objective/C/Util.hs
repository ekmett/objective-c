{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Objective.C.Util
  ( Nil(..)
  , Named(..)
  ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Foreign.ForeignPtr.Unsafe as Unsafe
import qualified System.IO.Unsafe as Unsafe

class Nil a where
  nil :: a
  isNil  :: a -> Bool

instance Nil (Ptr a) where
  nil = nullPtr
  isNil p = nullPtr == p

instance Nil (ForeignPtr a) where
  nil = Unsafe.unsafePerformIO $ newForeignPtr_ nullPtr
  isNil fp = isNil (Unsafe.unsafeForeignPtrToPtr fp)

instance Nil (FunPtr a) where
  nil = nullFunPtr
  isNil p = nullFunPtr == p

class Named a where
  name :: a -> String

instance Named String where
  name = id
