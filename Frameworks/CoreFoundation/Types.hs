{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Frameworks.CoreFoundation.Types
  ( CFTypeID
  , CFOptionFlags
  , CFHashCode
  , CFIndex
  , CFTypeRef
  , CFStringRef, ObjcCFString
  -- * current Core Foundation version
  , version
  ) where

import Data.Data
import Data.Functor
-- import Objective.C.Prim
import Objective.C
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import qualified System.IO.Unsafe as Unsafe

type CFTypeID      = CULong
type CFOptionFlags = CULong
type CFHashCode    = CULong
type CFIndex       = CLong
type CFTypeRef     = CId    -- toll-free-bridged

data ObjcCFString deriving Typeable
type CFStringRef = Ptr ObjcCFString

foreign import ccall "&kCFCoreFoundationVersionNumber" p_version :: Ptr CDouble

version :: Double
version = Unsafe.unsafePerformIO $ realToFrac <$> peek p_version
