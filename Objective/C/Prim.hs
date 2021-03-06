{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Primitive types used for FFI to objective C
module Objective.C.Prim
  (
  -- * Objective-C Primitives
  -- ** Booleans
    CBool, toBool, fromBool
  -- ** Identities
  , ForeignId, CId, ObjcObject
  -- ** Classes
  , ObjcClass
  -- ** Selectors
  , ObjcSelector
  -- ** Ivars
  , CIvar, ObjcIvar
  -- ** Protocols
  , ObjcProtocol
  ) where

import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

-- | native objective c boolean
type CBool = CSChar

-- | to haskell from an objective c boolean
toBool :: CBool -> Bool
toBool = toEnum . fromIntegral

-- | to objective c from a haskell boolean
fromBool :: Bool -> CBool
fromBool = fromIntegral . fromEnum

-- | an opaque objective C selector struct
data ObjcSelector deriving Typeable

-- | a (semi-opaque) objective C object, known to have an isa field linking to the Class
data ObjcObject deriving Typeable

-- | An objective C 'id'
type CId = Ptr ObjcObject

-- | An objective C 'id' with potential finalizers
type ForeignId = ForeignPtr ObjcObject

-- | an opaque objective c class struct
data ObjcClass deriving Typeable

-- | an opaque objective c ivar struct
data ObjcIvar deriving Typeable
type CIvar = Ptr ObjcIvar

-- | an opaque objective c protocol struct
data ObjcProtocol deriving Typeable
