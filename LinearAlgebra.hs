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

module LinearAlgebra
  ( Vector
  , Angle
  ) where

import Data.List (nub, transpose)

-- * Vectors -------------------------------------------------------------------

data Vector = V2 Float Float       -- Vector on plane
            | V3 Float Float Float -- Vector in space
            | S  Float             -- Real scalar

type Angle = Float -- Angle in radians


instance Show Vector where
  show (V2 x y)   = "Vector2(" ++ show x ++ ", " ++ show y ++ ")"
  show (V3 x y z) = "Vector3(" ++ show x ++ ", " ++ show y ++ ", " ++ show z
                 ++ ")"
  show (S s)      =  "Scalar(" ++ show s ++ ")"


instance Num Vector where
  -- Vector addition
  V2 x1 y1    + V2 x2 y2    = V2 (x1 + x2) (y1 + y2)
  V3 x1 y1 z1 + V3 x2 y2 z2 = V3 (x1 + x2) (y1 + y2) (z1 + z2)
  S  a        + S  b        = S  (a + b)
  _           + _           = error "Illegal Addition"

  -- Vector2 multiplication with scalar
  S a    * V2 x y = V2 (a*x) (a*y)
  V2 x y * S a    = V2 (a*x) (a*y)

  -- Vector3 multiplication with scalar
  S  a     * V3 x y z = V3 (a*x) (a*y) (a*z)
  V3 x y z * S  a     = V3 (a*x) (a*y) (a*z)

  -- Scalar multiplication
  S a * S b = S (a * b)

  _ * _ = error "Illegal Multiplication"

  -- Length of vector (assuming they exist in an orthonormalized base)
  abs (V2 x y)   = S (sqrt (x*x + y*y))
  abs (V3 x y z) = S (sqrt (x*x + y*y + z*z))
  -- Absolute of real number
  abs (S a) | a >= 0    = S a
            | otherwise = S (-a)

  -- Inverting a vector and scalar
  negate (V2 x y)   = V2 (-x) (-y)
  negate (V3 x y z) = V3 (-x) (-y) (-z)
  negate (S a)      = S  (-a)

  fromInteger a = S (fromInteger a)

  signum a = undefined


-- Example vectors

v2 :: Vector
v2 = V2 3 (-2)

v3 :: Vector
v3 = V3 (-2) (-1) 2


-- Conversion functions

toFloat :: Vector -> Float
toFloat (S a) = a
toFloat _     = error "Cannot convert non-scalar Vector to float"

toScalar :: Float -> Vector
toScalar = S

vToList :: Vector -> [Float]
vToList (V3 a b c) = [a, b, c]
vToList (V2 a b)   = [a, b]
vToList (S a)      = [a]


-- Dot product with angle specified.
dotAngle :: Vector -> Vector -> Angle -> Vector
dotAngle v1 v2 a = S (lv1 * lv2 * cos a)
  where
    (S lv1) = abs v1
    (S lv2) = abs v2


-- | Dot product. Assuming an orthonormalized base.
dot :: Vector -> Vector -> Vector
dot (V2 a1 b1)    (V2 a2 b2)    = S (a1*a2 + b1*b2)
dot (V3 a1 b1 c1) (V3 a2 b2 c2) = S (a1*a2 + b1*b2 + c1*c2)

dot _ _ = error $ "Invalid Input: Vectors must be of same dimension "
               ++ "and no scalars allowed."


-- | Cross product. Assuming an orthonormalized base.
cross :: Vector -> Vector -> Vector
cross (V3 a b c) (V3 x y z) = V3 newX newY newZ
  where
    newX = b*z - c*y
    newY = c*x - a*z
    newZ = a*y - b*x

cross _ _ = error "Invalid Input: Only use 3-dimensional vectors"

crossLen :: Vector -> Vector -> Angle -> Vector
crossLen v1 v2 n = abs v1 * abs v2 * toScalar n


-- * Matrices ------------------------------------------------------------------

-- Standard matrix
type WideVector =   [Float]
newtype Matrix  = M [WideVector]
type MatType    =   (Int, Int)

-- Fixed 2x2 matrix
newtype Matrix2 = M2 ( Float, Float,
                       Float, Float )


instance Show Matrix2 where
  show (M2 (a, b, c, d)) = "| " ++ show a ++ " " ++ show b ++ " |\n"
                        ++ "| " ++ show c ++ " " ++ show d ++ " |"

instance Show Matrix where
  show (M xs) = allRows xs
    where
      allRows []     = ""
      allRows (x:xs) = "| " ++ allCols x ++ "|\n" ++ allRows xs

      allCols [] = ""
      allCols (x:xs) = show x ++ " " ++ allCols xs


-- | Find the inverse a matrix.
findInverse :: Matrix2 -> IO ()
findInverse mat@(M2 (a, b, c, d)) = do
  let dd = getDeterminant mat
  let newMat = M2 (dd*d, dd*(-b), dd*(-c), dd*a)

  print newMat

-- | Get the determinant of a matrix.
getDeterminant :: Matrix2 -> Float
getDeterminant (M2 (a, b, c, d)) = if   dd == 0
                                   then error "No inverse"
                                   else 1 / dd
  where
    dd = a*d - c*b


-- Example matrices

m, invalidMatrix :: Matrix
m = M [ [ 1, 2, 3],
        [-1, 3, 6] ]

invalidMatrix = M [ [ 1, 2, 3],
                    [-1, 3, 6, 8] ]


-- | Invariant: Determine if the matrix is a rectangle and not empty.
isValidMatrix :: Matrix -> Bool
isValidMatrix (M xs)
  | null xs        = False
  | null (head xs) = False
  | otherwise      = do
    let ls     = map length xs
    let nubbed = nub ls

    length nubbed == 1

-- | Transpose a matrix.
transposeMatrix :: Matrix -> Matrix
transposeMatrix (M xs) = M (transpose xs)

-- | Get the dimensions of the matrix.
matrixSize :: Matrix -> MatType
matrixSize m@(M xs)
  | not (isValidMatrix m) = error "Invalid Matrix"
  | otherwise             = (length xs, length (head xs))

-- | Multiply matrices.
matMul :: Matrix -> Matrix -> Matrix
matMul m n
  | not (isValidMatrix m) = error "First Matrix Invalid"
  | not (isValidMatrix n) = error "Second Matrix Invalid"
  | colsM /= rowsN        = error "Matrix multiplication not defined"

  | otherwise = undefined
  where
    (_, colsM) = matrixSize m
    (rowsN, _) = matrixSize n

vectorsToMatrix :: Vector -> Vector -> Vector -> Matrix
vectorsToMatrix v1 v2 v3 = transposeMatrix $ M [nV1, nV2, nV3]
  where
    nV1 = vToList v1
    nV2 = vToList v2
    nV3 = vToList v3
