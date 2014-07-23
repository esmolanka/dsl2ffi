{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import TH
import Expr

fun2ffi ( Function "fooo" ["x", "b"] [Bind "y" (Add 2 (Ref "x" * Lit (10 :: Double))), Return (Mul (Ref "y") (Ref "b"))] )

main :: IO ()
main = print $ fooo 5 2.5
