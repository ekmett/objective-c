{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Objective.C.Runtime
  (
  -- * Classes
    Class(..), GetClass(..)
  , getSuperclass, isMetaClass
  , getVersion, setVersion
  , getInstanceSize
  -- * Object Identities
  , Id(..)
  , Nil(..)
  -- * Selectors
  , Selector(..)
  , selector
  , selectorName
  , selectorIsMapped
  -- * Unsafe core
  , HasId(..)
  , copy
  ) where

import Control.Applicative
import Data.Data
import Data.Hashable
import qualified System.IO.Unsafe as Unsafe
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe as Unsafe
import Foreign.Ptr
import Objective.C.Prim
import Text.Read

newtype Class = Class { classPtr :: Ptr ObjcClass } deriving (Eq,Ord,Data,Typeable)
instance Hashable Class where
  hashWithSalt n (Class p) = n `hashWithSalt` (fromIntegral (ptrToIntPtr p) :: Int)

newtype Id = Id { foreignId :: ForeignPtr ObjcObject } deriving (Eq,Ord,Data,Typeable)
instance Hashable Id where
  hashWithSalt n (Id p) = hashWithSalt n (fromIntegral (ptrToIntPtr (Unsafe.unsafeForeignPtrToPtr p)) :: Int)

class GetClass a where
  getClass :: a -> IO Class
  default getClass :: HasId a => a -> IO Class
  getClass a = Class <$> withId a c_object_getClass

  getClassName :: a -> IO String
  default getClassName :: HasId a => a -> IO String
  getClassName a = withId a c_object_getClassName >>= peekCString

  getMetaClass :: a -> IO Class
  default getMetaClass :: HasId a => a -> IO Class
  getMetaClass a = getClass a >>= getClass

foreign import ccall unsafe "objc/runtime.h object_getClass"     c_object_getClass     :: CId -> IO CClass
foreign import ccall unsafe "objc/runtime.h object_getClassName" c_object_getClassName :: CId -> IO CString

instance GetClass Id
instance GetClass Class
instance GetClass String where
  getClass name = Class <$> withCString name c_objc_getClass
  getClassName = return
  getMetaClass name = Class <$> withCString name c_objc_getMetaClass
foreign import ccall unsafe "objc/runtime.h objc_getClass"     c_objc_getClass     :: CString -> IO CClass
foreign import ccall unsafe "objc/runtime.h objc_getMetaClass" c_objc_getMetaClass :: CString -> IO CClass

class GetClass a => SetClass a where
  setClass :: a -> Class -> IO Class
  default setClass :: HasId a => a -> Class -> IO Class
  setClass a (Class c) = withId a $ \p -> Class <$> c_object_setClass p c
foreign import ccall unsafe "objc/runtime.h object_setClass"     c_object_setClass     :: CId -> CClass -> IO CClass

instance SetClass Class
instance SetClass Id

class GetClass a => HasId a where
  withId :: a -> (CId -> IO b) -> IO b

instance HasId Class where
  withId (Class p) f = f (castPtr p)

instance HasId Id where
  withId (Id fp) = withForeignPtr fp

class Nil a where
  nil   :: a
  isNil :: a -> Bool

instance Nil Id where
  nil = nilId
  isNil (Id fp) = Unsafe.unsafeForeignPtrToPtr fp == nullPtr

nilId :: Id
nilId = Unsafe.unsafePerformIO $ Id <$> newForeignPtr_ nullPtr
{-# NOINLINE nilId #-}

instance Nil Class where
  nil = Class nullPtr
  isNil (Class p) = p == nullPtr

getSuperclass :: Class -> IO Class
getSuperclass (Class c) = Class <$> c_class_getSuperclass c
foreign import ccall unsafe "objc/runtime.h class_getSuperclass" c_class_getSuperclass :: CClass -> IO CClass

isMetaClass :: Class -> IO Bool
isMetaClass (Class c) = toBool <$> c_class_isMetaClass c
foreign import ccall unsafe "objc/runtime.h class_isMetaClass" c_class_isMetaClass :: CClass -> IO CBool

getInstanceSize :: Class -> IO CSize
getInstanceSize (Class c) = c_class_getInstanceSize c
foreign import ccall unsafe "objc/runtime.h class_getInstanceSize" c_class_getInstanceSize :: CClass -> IO CSize

getVersion :: Class -> IO Int
getVersion (Class c) = fromIntegral <$> c_class_getVersion c
foreign import ccall unsafe "objc/runtime.h class_getVersion"      c_class_getVersion :: CClass -> IO CInt

setVersion :: Class -> Int -> IO ()
setVersion (Class c) i = c_class_setVersion c (fromIntegral i)
foreign import ccall unsafe "objc/runtime.h class_setVersion" c_class_setVersion :: CClass -> CInt -> IO ()

-- unsafe bits:
copy :: Id -> CSize -> IO Id
copy a s = withId a $ \p -> do
  p' <- c_object_copy p s
  fp' <- newForeignPtr_ p'
  return $ Id fp'

foreign import ccall unsafe "objc/runtime.h object_copy" c_object_copy :: CId -> CSize -> IO CId

-- nothing so far has needed a single selector

foreign import ccall unsafe "objc/objc.h sel_getName"      c_sel_getName      :: CSel -> CString
foreign import ccall unsafe "objc/objc.h sel_registerName" c_sel_registerName :: CString -> IO CSel

newtype Selector = Selector (Ptr ObjcSelector) deriving (Eq,Ord,Typeable,Data)

instance Nil Selector where
  nil = Selector nullPtr
  isNil (Selector p) = p == nullPtr

-- foreign import ccall unsafe "objc/objc.h sel_isEqual" c_sel_isEqual :: CSel -> CSel -> CBool
-- instance Eq Sel where Sel s == Sel t = toBool (c_sel_isEqual s t)
-- instance Ord Sel where compare = compare `on` selectorName

instance Hashable Selector where
  hashWithSalt n (Selector p) = n `hashWithSalt` (fromIntegral (ptrToIntPtr p) :: Int)

instance Show Selector where
  showsPrec d s = showParen (d > 10) $ showString "selector " . showsPrec 10 (selectorName s)

instance Read Selector where
  readPrec = parens $ prec 10 $ do
    Ident "selector" <- lexP
    s <- readPrec
    return $! selector s

-- | Find a selector by name, @selector "foo:"@ is analogous to @\@selector(foo:)@ in Objective C.
selector :: String -> Selector
selector s = Unsafe.unsafePerformIO $ Selector <$> withCString s c_sel_registerName

selectorName :: Selector -> String
selectorName (Selector s) = Unsafe.unsafePerformIO $ peekCString $ c_sel_getName s

selectorIsMapped :: Selector -> IO Bool
selectorIsMapped (Selector s) = toBool <$> c_sel_isMapped s
foreign import ccall unsafe "objc/objc.h sel_isMapped"     c_sel_isMapped     :: CSel -> IO CBool
