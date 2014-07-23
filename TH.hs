{-# LANGUAGE TemplateHaskell #-}

module TH where

import System.Process
import Language.Haskell.TH
import Expr

class GetHType ty where
    getHType :: proxy ty -> Type

instance GetHType Double where
    getHType _ = ConT ''Double

instance GetHType Int where
    getHType _ = ConT ''Int

fun2ffi :: (GetType a, GetHType a, Pretty a) => Function a -> Q [Dec]
fun2ffi fn@(Function name args _) = do
  runIO $ do writeFile "code.c" (pretty fn)
             writeFile "code.h" (pretty (Declaration fn))
             rawSystem "gcc" ["-o", "code.o", "-c", "code.c"]
  n <- newName name
  let ty = argsToTypes (getHType fn) (length args)
  return [ ForeignD $ ImportF CCall Safe ("code.h " ++ name) n ty ]

argsToTypes :: Type -> Int -> Type
argsToTypes ty n = go n
    where
      go 0 = ty
      go n = AppT (AppT ArrowT ty) (go (n-1))
