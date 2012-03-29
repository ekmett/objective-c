{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE PolyKinds, ConstraintKinds, DataKinds, KindSignatures, FlexibleInstances, ExtendedDefaultRules, EmptyDataDecls 
-- | Primitive types used for FFI to objective C
module Objective.C.Prim
  (
  -- * Objective-C Primitives
  -- ** Identities
    ForeignId, CId, ObjcObject
  -- ** Classes
  , CClass, ObjcClass
  -- ** Selectors
  , CSel, ObjcSelector
  -- ** Ivars
  , CIvar, ObjcIvar
  -- ** Booleans
  , CBool, toBool, fromBool
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
-- | SEL as used by objective C
type CSel = Ptr ObjcSelector

-- | a (semi-opaque) objective C object, known to have an isa field linking to the Class
data ObjcObject deriving Typeable
-- | An objective C 'id'
type CId = Ptr ObjcObject
-- | An objective C 'id' with potential finalizers
type ForeignId = ForeignPtr ObjcObject

-- | an opaque objective c class struct
data ObjcClass deriving Typeable
type CClass = Ptr ObjcClass

-- | an opaque objective c ivar struct
data ObjcIvar deriving Typeable
type CIvar = Ptr ObjcIvar
