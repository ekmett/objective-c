{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module Objective.C.Selector
  (
  -- * Selectors
    Selector(..)
  , isMapped
  -- * Arbitrary selectors
  , Sel(..)
  ) where

import Control.Monad
import Data.Data
import Data.Hashable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Objective.C.Prim
import Objective.C.Util
import qualified System.IO.Unsafe as Unsafe
import Text.Read

foreign import ccall unsafe "objc/objc.h" sel_isMapped :: Sel -> IO CBool
foreign import ccall unsafe "objc/objc.h" sel_getName  :: Sel -> CString
foreign import ccall unsafe "objc/objc.h" sel_getUid   :: CString -> IO Sel

-- Selectors
class Named a => Selector a where
  sel :: a -> Sel

isMapped :: Selector a => a -> IO Bool
isMapped = fmap toBool . sel_isMapped . sel

instance Selector String where
  sel n = Unsafe.unsafePerformIO $ withCString n sel_getUid

-- An unknown Selector
newtype Sel = Sel (Ptr ObjcSelector) deriving (Eq,Ord,Typeable,Data,Nil,Storable)

instance Hashable Sel where
  hashWithSalt n (Sel s) = n `hashWithSalt` (fromIntegral (ptrToIntPtr s) :: Int)

instance Named Sel where
  name = Unsafe.unsafePerformIO . peekCString . sel_getName

instance Selector Sel where
  sel = id

instance Show Sel where
  showsPrec d s
    | isNil s = showString "nil"
    | otherwise = showParen (d > 10) $ showString "sel " . showsPrec 10 (name s)

instance Read Sel where
  readPrec = parens
    ( prec 10 $ do
        Ident "sel" <- lexP
        s <- readPrec
        return $! sel (s :: String)
    `mplus` do
        Ident "nil" <- lexP
        return nil
    )
