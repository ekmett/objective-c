{-# LANGUAGE TemplateHaskell #-}
module Objective.C.Fun.TH
  ( makeFun
  , makeFuns
  ) where

import Language.Haskell.TH

-- > foreign import ccall "dynamic" dynamicFoo :: FunPtr Foo -> IO Foo
-- > foreign import ccall "wrapper" wrapFoo :: Foo -> IO (FunPtr Foo)
-- > instance Fun Foo where
-- >   dynamic = dynamicFoo
-- >   wrap    = wrapFoo

infixr 9 ->:
(->:) :: Type -> Type -> Type
a ->: b = ArrowT `AppT` a `AppT` b

funPtr :: Type -> Type
funPtr a = ConT (mkName "FunPtr") `AppT` a

io :: Type -> Type
io a = ConT (mkName "IO") `AppT` a

fun :: Type -> Type
fun a = ConT (mkName "Fun") `AppT` a

makeFun :: String -> Type -> Q [Dec]
makeFun s t = do
  dynamicName <- newName $ "dynamic" ++ s 
  wrapName    <- newName $ "wrap" ++ s
  return [ ForeignD (ImportF CCall Unsafe "dynamic" dynamicName (funPtr t ->: io t))
         , ForeignD (ImportF CCall Unsafe "wrapper" wrapName (t ->: io (funPtr t)))
         , InstanceD [] (fun t) [ FunD (mkName "dynamic") [Clause [] (NormalB (VarE dynamicName)) []]
                                , FunD (mkName "wrap")    [Clause [] (NormalB (VarE wrapName)) []]]]

makeFuns :: [(String, Type)] -> Q [Dec]
makeFuns xs = do
  xss <- mapM (uncurry makeFun) xs
  return $ concat xss
