{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Objective.C.Runtime
  (
    Nil(..)
  -- * Classes
  , Class(..)
  , getInstanceSize
  , getVersion
  , setVersion
  -- * Superclasses
  , Super
  , getSuperclass
  -- * MetaClasses
  , Meta
  , isMetaClass
  -- * Objects
  , Id(..), getClassOf, getClassNameOf
  -- * Selectors
  , Selector(..), isMapped
  -- * Strongly Typed ClassPairs
  , ClassPair(..)
  -- * Arbitrary selectors
  , Sel, sel
  -- * Arbitrary objects
  , Object, Class_, Meta_
  , setClass
  , getClassByName
  , getMetaClassByName
  , copyObject
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

-- | Nilable types
class Nil a where
  nil   :: a

-- | Classes
newtype Class a = Class { cclass :: CClass } deriving (Eq,Ord,Typeable,Data)

instance Hashable (Class a) where
  hashWithSalt n (Class p) = n `hashWithSalt` (fromIntegral (ptrToIntPtr p) :: Int)

instance Nil (Class a) where
  nil = Class nullPtr

getInstanceSize :: Class a -> IO CSize
getInstanceSize (Class c) = c_class_getInstanceSize c
foreign import ccall unsafe "objc/runtime.h class_getInstanceSize" c_class_getInstanceSize :: CClass -> IO CSize

getVersion :: Class a -> IO Int
getVersion (Class c) = fromIntegral <$> c_class_getVersion c
foreign import ccall unsafe "objc/runtime.h class_getVersion" c_class_getVersion :: CClass -> IO CInt

setVersion :: Class a -> Int -> IO ()
setVersion (Class c) i = c_class_setVersion c (fromIntegral i)
foreign import ccall unsafe "objc/runtime.h class_setVersion" c_class_setVersion :: CClass -> CInt -> IO ()

type Meta a = Class (Class a)
isMetaClass :: Class a -> Bool
isMetaClass (Class c) = toBool (c_class_isMetaClass c)
foreign import ccall unsafe "objc/runtime.h class_isMetaClass" c_class_isMetaClass :: CClass -> CBool

type family Super a
getSuperclass :: Class a -> IO (Class (Super a))
getSuperclass (Class c) = Class <$> c_class_getSuperclass c
foreign import ccall unsafe "objc/runtime.h class_getSuperclass" c_class_getSuperclass :: CClass -> IO CClass

-- | Representations of foreign objects
class Nil a => Id a where
  withId :: a -> (Ptr ObjcObject -> IO b) -> IO b
  isNil  :: a -> Bool

instance Id (Class a) where
  withId (Class cp) f = f (castPtr cp)
  isNil (Class cp) = cp == nullPtr

getClassOf :: Id a => a -> IO (Class a)
getClassOf a = Class <$> withId a c_object_getClass
foreign import ccall unsafe "objc/runtime.h object_getClass"     c_object_getClass     :: CId -> IO CClass

getClassNameOf :: Id a => a -> IO String
getClassNameOf a = withId a c_object_getClassName >>= peekCString
foreign import ccall unsafe "objc/runtime.h object_getClassName" c_object_getClassName :: CId -> IO CString

-- Selectors
class Selector a where
  selectorName :: a -> String
  csel         :: a -> CSel

isMapped :: Selector a => a -> IO Bool
isMapped s = toBool <$> c_sel_isMapped (csel s)
foreign import ccall unsafe "objc/objc.h sel_isMapped" c_sel_isMapped :: CSel -> IO CBool

instance Selector String where
  selectorName = id
  csel n = Unsafe.unsafePerformIO $ withCString n c_sel_registerName
foreign import ccall unsafe "objc/objc.h sel_registerName" c_sel_registerName :: CString -> IO CSel

-- An Unknown Selector
newtype Sel = Sel CSel deriving (Eq,Ord,Typeable,Data)

instance Selector Sel where
  selectorName (Sel s) = Unsafe.unsafePerformIO $ peekCString $ c_sel_getName s
  csel (Sel p) = p
foreign import ccall unsafe "objc/objc.h sel_getName"      c_sel_getName      :: CSel -> CString

instance Show Sel where
  showsPrec d s = showParen (d > 10) $ showString "sel " . showsPrec 10 (selectorName s)

instance Read Sel where
  readPrec = parens $ prec 10 $ do
    Ident "sel" <- lexP
    (s :: String) <- readPrec
    return $! sel s

sel :: Selector a => a -> Sel
sel a = Sel (csel a)


class ClassPair a where
  getClassNameByProxy :: p a -> String

  getClass :: Class a
  getClass = r where
    r = Unsafe.unsafePerformIO $ Class <$> withCString (getClassNameByProxy r) c_objc_getClass

{-
  getMetaClass :: Class (Class a)
  getMetaClass = r where
    r = Unsafe.unsafePerformIO $ Class <$> withCString (getClassNameByProxy (argOf r)) c_objc_getMetaClass
    argOf :: a -> f a -> a
    argOf a _ = a
-}
foreign import ccall unsafe "objc/runtime.h objc_getClass" c_objc_getClass :: CString -> IO CClass
foreign import ccall unsafe "objc/runtime.h objc_getMetaClass" c_objc_getMetaClass :: CString -> IO CClass

-- Objects with unknown classes
newtype Object = Object (ForeignPtr ObjcObject) deriving (Eq,Ord,Show,Data,Typeable)

instance Hashable Object where
  hashWithSalt n (Object fp) = hashWithSalt n (fromIntegral (ptrToIntPtr (Unsafe.unsafeForeignPtrToPtr fp)) :: Int)

instance Nil Object where
  nil = nilObject

nilObject :: Object
nilObject = Unsafe.unsafePerformIO $ Object <$> newForeignPtr_ nullPtr
{-# NOINLINE nilObject #-}

instance Id Object where
  withId (Object fp) = withForeignPtr fp
  isNil (Object fp) = Unsafe.unsafeForeignPtrToPtr fp == nullPtr

type Class_ = Class Object
type Meta_ = Meta Object

getClassByName :: String -> IO (Class Object)
getClassByName name = Class <$> withCString name c_objc_getClass

getMetaClassByName :: String -> IO (Meta Object)
getMetaClassByName name = Class <$> withCString name c_objc_getMetaClass

setClass :: Object -> Class a -> IO (Class a)
setClass a (Class c) = withId a $ \p -> Class <$> c_object_setClass p c
foreign import ccall unsafe "objc/runtime.h object_setClass" c_object_setClass :: CId -> CClass -> IO CClass

copyObject :: Id a => a -> CSize -> IO Object
copyObject a s = withId a $ \p -> do
  p' <- c_object_copy p s
  fp' <- newForeignPtr_ p'
  return $ Object fp'
foreign import ccall unsafe "objc/runtime.h object_copy" c_object_copy :: CId -> CSize -> IO CId
