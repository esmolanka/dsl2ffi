{-# LANGUAGE GADTs #-}
module Expr where

import Data.List

type Ident = String

data Expr a where
    Add :: (Num a) => Expr a -> Expr a -> Expr a
    Mul :: (Num a) => Expr a -> Expr a -> Expr a
    Ref :: Ident -> Expr a
    Lit :: a -> Expr a

test :: Expr Integer
test = Add (Lit 10) (Mul (Lit 10) (Ref "x"))

test2 :: Expr Double
test2 = Add (Lit 10) (Mul (Lit 10) (Ref "x"))

class Pretty a where
    pretty :: a -> String

instance Pretty Int where
    pretty = show

instance Pretty Double where
    pretty = show

instance Pretty a => Pretty (Expr a) where
    pretty (Add e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
    pretty (Mul e1 e2) = "(" ++ pretty e1 ++ " * " ++ pretty e2 ++ ")"
    pretty (Ref v)     = v
    pretty (Lit x)     = pretty x

data Statement a = Bind Ident (Expr a)
                 | Return (Expr a)

instance (GetType a, Pretty a) => Pretty (Statement a) where
    pretty (Bind ident expr) = pretty (getType expr) ++ " " ++ ident ++ " = " ++ pretty expr ++ ";"
    pretty (Return expr)     = "return " ++ pretty expr ++ ";"

data CType a where
    CTInt :: CType Int
    CTDouble :: CType Double

instance Pretty (CType a) where
    pretty CTInt = "int"
    pretty CTDouble = "double"

class GetType ty where
    getType :: proxy ty -> CType ty

instance GetType Double where
    getType _ = CTDouble

instance GetType Int where
    getType _ = CTInt

data Function a = Function Ident [Ident] [Statement a]

newtype Declaration a = Declaration (Function a)

instance (GetType a, Pretty a) => Pretty (Function a) where
    pretty fn@(Function name args stmts) = concat
      [ tyStr, " "
      , name
      ,  "("
      , intercalate "," . map prettyArg $ args
      , ") "
      , "{\n"
      , intercalate "\n" . map (("  "++) . pretty) $ stmts
      , "\n}"
      ]
        where prettyArg name = tyStr ++ " " ++ name
              tyStr = pretty $ getType fn

instance (GetType a, Pretty a) => Pretty (Declaration a) where
    pretty (Declaration fn@(Function name args _)) = concat
      [ tyStr, " "
      , name
      ,  "("
      , intercalate "," . map prettyArg $ args
      , ");"
      ]
        where prettyArg name = tyStr ++ " " ++ name
              tyStr = pretty $ getType fn

instance Num a => Num (Expr a) where
    (+) a b = Add a b
    (-) a b = Add a (Mul (Lit (-1)) b)
    (*) a b = Mul a b
    fromInteger = Lit . fromIntegral
    abs = undefined
    signum = undefined


test3 :: Function Double
test3 = Function "foo" [("x")] [Bind "y" test2, Return (Ref "y")]

test4 :: Function Int
test4 = Function "bar" ["x"] [Bind "y" (2 + Ref "x" * 10), Return (Ref "y" * 2)]

