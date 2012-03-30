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
  , Sel, sel
  ) where

import Control.Monad
import Control.Applicative
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

foreign import ccall unsafe "objc/objc.h sel_isMapped" c_sel_isMapped :: CSel -> IO CBool
foreign import ccall unsafe "objc/objc.h sel_getName"  c_sel_getName  :: CSel -> CString
foreign import ccall unsafe "objc/objc.h sel_getUid"   c_sel_getUid   :: CString -> IO CSel

-- Selectors
class Named a => Selector a where
  csel :: a -> CSel

isMapped :: Selector a => a -> IO Bool
isMapped s = toBool <$> c_sel_isMapped (csel s)

instance Selector String where
  csel n = Unsafe.unsafePerformIO $ withCString n c_sel_getUid

-- An unknown Selector
newtype Sel = Sel CSel deriving (Eq,Ord,Typeable,Data,Nil,Storable)

instance Hashable Sel where
  hashWithSalt n (Sel s) = n `hashWithSalt` (fromIntegral (ptrToIntPtr s) :: Int)

instance Named Sel where
  name (Sel s) = Unsafe.unsafePerformIO $ peekCString $ c_sel_getName s

instance Selector Sel where
  csel (Sel p) = p

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

sel :: Selector a => a -> Sel
sel a = Sel (csel a)
