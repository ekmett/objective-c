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

foreign import ccall unsafe "objc/runtime.h" class_addProtocol           :: Class a -> Protocol -> IO ()
foreign import ccall unsafe "objc/runtime.h" class_conformsToProtocol    :: Class a -> Protocol -> IO CBool
foreign import ccall unsafe "objc/runtime.h" class_copyProtocolList      :: Class a -> Ptr CInt -> IO (Ptr Protocol)
foreign import ccall unsafe "objc/runtime.h" objc_allocateProtocol       :: CString -> IO Protocol
foreign import ccall unsafe "objc/runtime.h" objc_copyProtocolList       :: Ptr CInt -> IO (Ptr Protocol)
foreign import ccall unsafe "objc/runtime.h" objc_getProtocol            :: CString -> IO Protocol
foreign import ccall unsafe "objc/runtime.h" protocol_addProtocol        :: Protocol -> Protocol -> IO ()
foreign import ccall unsafe "objc/runtime.h" protocol_conformsToProtocol :: Protocol -> Protocol -> IO CBool
foreign import ccall unsafe "objc/runtime.h" protocol_copyProtocolList   :: Protocol -> Ptr CInt -> IO (Ptr Protocol)
foreign import ccall unsafe "objc/runtime.h" protocol_getName            :: Protocol -> IO CString

-- | register the protocol with the Objective C runtime
foreign import ccall unsafe "objc/runtime.h objc_registerProtocol" registerProtocol :: Protocol -> IO ()

newtype Protocol = Protocol (Ptr ObjcProtocol) deriving (Eq,Ord,Data,Typeable,Nil,Storable)

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
  name p = Unsafe.unsafePerformIO $ protocol_getName p >>= peekCString

-- | Retrieve a protocol with a given name, or nil if it isn't present
getProtocol :: String -> IO Protocol
getProtocol n = withCString n objc_getProtocol

-- | Only used internally, assumes that the protocol already exists. If this protocol isn't being
-- supplied by an external framework, use getProtocol to be safe.
protocol :: String -> Protocol
protocol = Unsafe.unsafePerformIO . getProtocol

-- | return the current global protocol list
protocols :: IO [Protocol]
protocols = copyProtocols objc_copyProtocolList

-- | allocate space for a new protocol
allocateProtocol :: String -> IO Protocol
allocateProtocol n = withCString n objc_allocateProtocol

class HasProtocols t where
  conformsToProtocol :: t -> Protocol -> IO Bool
  protocolsOf        :: t -> IO [Protocol]
  addProtocol        :: t -> Protocol -> IO ()

instance HasProtocols (Class a) where
  conformsToProtocol c p = toBool <$> class_conformsToProtocol c p
  protocolsOf c = copyProtocols (class_copyProtocolList c)
  addProtocol = class_addProtocol

instance HasProtocols Protocol where
  conformsToProtocol p q = toBool <$> protocol_conformsToProtocol p q
  protocolsOf p = copyProtocols (protocol_copyProtocolList p)
  addProtocol = protocol_addProtocol

-- private helper
copyProtocols :: (Ptr CInt -> IO (Ptr Protocol)) -> IO [Protocol]
copyProtocols f = alloca $ \p -> do
  ps <- f p
  count <- peek p
  xs <- forM [0..fromIntegral count-1] $ peekElemOff ps
  free ps
  return xs
