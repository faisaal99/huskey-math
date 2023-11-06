{-
Copyright (C) 2023 Faisal A.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the “Software”), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Logic
  ( BoolOp
  , BooleanExpr

  , bool
  , and
  , or
  , xor
  , eval
  ) where

import Prelude hiding (and, or)


-- * Data ----------------------------------------------------------------------

-- | A boolean operation on two boolean values.
data BoolOp = And
            | Or

-- | TODO:
data BooleanExpr = B Bool
                 | Expr BooleanExpr BoolOp BooleanExpr
                 | Not BooleanExpr

instance Show BooleanExpr where
  show (B False) = "F"
  show (B True)  = "T"

  show (Not (B False)) = "T"
  show (Not (B True))  = "F"
  show (Not b)         = "!(" ++ show b ++ ")"

  show (Expr a And b) = showAnd a ++ "*" ++ showAnd b
  show (Expr a Or  b) = show a ++ " + " ++ show b

showAnd :: BooleanExpr -> String
showAnd e@(Expr _ Or  _) = "(" ++ show e ++ ")"
showAnd e                = show e


-- * Constructors --------------------------------------------------------------

bool :: Bool -> BooleanExpr
bool b = B b

and :: Bool -> Bool -> BooleanExpr
and a b = Expr (B a) And (B b)

or :: Bool -> Bool -> BooleanExpr
or a b = Expr (B a) Or (B b)

xor :: Bool -> Bool -> BooleanExpr
xor a b =
  Expr
    (Expr (B a) And (Not (B b)))
    Or
    (Expr (Not (B a)) And (B b))

deMorgan :: BooleanExpr -> BooleanExpr
deMorgan = undefined

eval :: BooleanExpr -> Bool
eval (B b)          = b
eval (Not b)        = not $ eval b
eval (Expr a And b) = eval a && eval b
eval (Expr a Or b)  = eval a || eval b
