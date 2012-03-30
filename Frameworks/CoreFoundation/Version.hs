{-# LANGUAGE ForeignFunctionInterface #-}
module Frameworks.CoreFoundation.Version
  (
  -- * current Core Foundation version
    version
  ) where

import Data.Functor
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import qualified System.IO.Unsafe as Unsafe

foreign import ccall "&kCFCoreFoundationVersionNumber" p_version :: Ptr CDouble

version :: Double
version = Unsafe.unsafePerformIO $ realToFrac <$> peek p_version
