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
  ) where

import Data.Data
import Objective.C.Prim
import Foreign.Ptr
import Foreign.C.Types

type CFTypeID      = CULong
type CFOptionFlags = CULong
type CFHashCode    = CULong
type CFIndex       = CLong
type CFTypeRef     = CId    -- toll-free-bridged

data ObjcCFString deriving Typeable
type CFStringRef = Ptr ObjcCFString
