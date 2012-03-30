{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Objective.C.Protocol
  ( Protocol(..)
  , protocol
  -- * checking protocols
  , protocols
  , HasProtocols(..)
  -- * creating protocols
  , allocateProtocol
  , registerProtocol
  , getProtocol
  ) where

import Control.Monad
import Data.Data
import Data.Functor
import Data.Hashable
import Objective.C.Prim
import Objective.C.Util
import Objective.C.Class
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified System.IO.Unsafe as Unsafe
import Text.Read

foreign import ccall unsafe "objc/runtime.h class_addProtocol"           c_class_addProtocol           :: CClass -> CProtocol -> IO ()
foreign import ccall unsafe "objc/runtime.h class_conformsToProtocol"    c_class_conformsToProtocol    :: CClass -> CProtocol -> IO CBool
foreign import ccall unsafe "objc/runtime.h class_copyProtocolList"      c_class_copyProtocolList      :: CClass -> Ptr CInt -> IO (Ptr CProtocol)
foreign import ccall unsafe "objc/runtime.h objc_allocateProtocol"       c_objc_allocateProtocol       :: CString -> IO CProtocol
foreign import ccall unsafe "objc/runtime.h objc_copyProtocolList"       c_objc_copyProtocolList       :: Ptr CInt -> IO (Ptr CProtocol)
foreign import ccall unsafe "objc/runtime.h objc_getProtocol"            c_objc_getProtocol            :: CString -> IO CProtocol
foreign import ccall unsafe "objc/runtime.h objc_registerProtocol"       c_objc_registerProtocol       :: CProtocol -> IO ()
foreign import ccall unsafe "objc/runtime.h protocol_addProtocol"        c_protocol_addProtocol        :: CProtocol -> CProtocol -> IO ()
foreign import ccall unsafe "objc/runtime.h protocol_conformsToProtocol" c_protocol_conformsToProtocol :: CProtocol -> CProtocol -> IO CBool
foreign import ccall unsafe "objc/runtime.h protocol_copyProtocolList"   c_protocol_copyProtocolList   :: CProtocol -> Ptr CInt -> IO (Ptr CProtocol)
foreign import ccall unsafe "objc/runtime.h protocol_getName"            c_protocol_getName            :: CProtocol -> IO CString

newtype Protocol = Protocol CProtocol deriving (Eq,Ord,Data,Typeable,Nil,Storable)

instance Hashable Protocol where
  hashWithSalt n (Protocol s) = n `hashWithSalt` (fromIntegral (ptrToIntPtr s) :: Int)

instance Show Protocol where
  showsPrec d s
    | isNil s = showString "nil"
    | otherwise = showParen (d > 10) $ showString "protocol " . showsPrec 10 (name s)

instance Read Protocol where
  readPrec = parens
    ( prec 10 $ do
        Ident "protocol" <- lexP
        s <- readPrec
        return $! protocol (s :: String)
       `mplus` do
        Ident "nil" <- lexP
        return nil
    )

instance Named Protocol where
  name (Protocol p) = Unsafe.unsafePerformIO $ c_protocol_getName p >>= peekCString

-- | Retrieve a protocol with a given name, or nil if it isn't present
getProtocol :: String -> IO Protocol
getProtocol n = Protocol <$> withCString n c_objc_getProtocol

-- | Only used internally, assumes that the protocol already exists. If this protocol isn't being
-- supplied by an external framework, use getProtocol to be safe.
protocol :: String -> Protocol
protocol = Unsafe.unsafePerformIO . getProtocol

-- | return the current global protocol list
protocols :: IO [Protocol]
protocols = copyProtocols c_objc_copyProtocolList

-- | allocate space for a new protocol
allocateProtocol :: String -> IO Protocol
allocateProtocol n = Protocol <$> withCString n c_objc_allocateProtocol

-- | register the protocol with the Objective C runtime
registerProtocol :: Protocol -> IO ()
registerProtocol (Protocol p) = c_objc_registerProtocol p

class HasProtocols t where
  conformsToProtocol :: t -> Protocol -> IO Bool
  protocolsOf        :: t -> IO [Protocol]
  addProtocol        :: t -> Protocol -> IO ()

instance HasProtocols (Class a) where
  conformsToProtocol (Class c) (Protocol p) = toBool <$> c_class_conformsToProtocol c p
  protocolsOf (Class c) = copyProtocols (c_class_copyProtocolList c)
  addProtocol (Class c) (Protocol p) = c_class_addProtocol c p

instance HasProtocols Protocol where
  conformsToProtocol (Protocol p) (Protocol q) = toBool <$> c_protocol_conformsToProtocol p q
  protocolsOf (Protocol p) = copyProtocols (c_protocol_copyProtocolList p)
  addProtocol (Protocol p) (Protocol q) = c_protocol_addProtocol p q

-- private helper
copyProtocols :: (Ptr CInt -> IO (Ptr CProtocol)) -> IO [Protocol]
copyProtocols f = alloca $ \p -> do
  ps <- f p
  count <- peek p
  xs <- forM [0..fromIntegral count-1] $ \i -> Protocol <$> peekElemOff ps i
  free ps
  return xs
