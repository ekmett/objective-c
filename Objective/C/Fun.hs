{-# LANGUAGE TemplateHaskell #-}
module Objective.C.Fun
  ( Fun(..)
  ) where

import Foreign.C.Types
import Foreign.Ptr
import Objective.C.Fun.TH
import Language.Haskell.TH

class Fun a where
  dynamic :: FunPtr a -> IO a
  wrap    :: a -> IO (FunPtr a)

-- a few primitive types to test with
makeFuns
   [ ("CInt",   ConT ''CInt)
   , ("CSize",  ConT ''CSize)
   , ("CDouble",ConT ''CDouble)
   , ("Unit",   ConT ''())
   ]
