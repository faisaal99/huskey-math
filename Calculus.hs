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

module Calculus
  ( Complex
  ) where


-- * Data ----------------------------------------------------------------------

-- | Defines a simple complex number.
data Complex = C (Float, Float) | CN Float

instance Show Complex where
  show (C (a, b))
    | b < 0     = show a ++ " - i" ++ show (abs b)
    | otherwise = show a ++ " + i" ++ show b
  show (CN n) = show n

instance Num Complex where
  C (a1, b1) + C (a2, b2) = C (a1+a2, b1+b2)
  CN a       + CN b       = CN (a + b)
  _          + _          = error "Illegal Addition"

  C (a1, b1) * C (a2, b2) = C (a1*a2 - b1*b2, a1*b2 + a2*b1)

  CN n       * C (a,  b)  = C (a*n, b*n)
  C (a,  b)  * CN n       = C (a*n, b*n)

  _          * _          = error "Illegal Multiplication"

  negate (C (a, b)) = C (-a, -b)
  negate (CN a) = CN (-a)

  abs (CN n) = CN (abs n)
  abs _      = error "I don't even know why"

  signum = undefined

  fromInteger n = CN (fromInteger n)


-- * Functions -----------------------------------------------------------------

-- Extract Re and Im from a complex number
re, im :: Complex -> Float
re (C (a, _)) = a
re _          = error "re error"

im (C (_, b)) = b
im _          = error "im error"
