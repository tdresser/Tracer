module Tracertypes (
  Color(Color), 
  Vector(Vector), 
  Ray(..), 
  Camera(..), 
  Quaternion(..),
  Sphere(Sphere),
  vlength,
  vtimes) where

data Color  = Color   Float Float Float  deriving (Show, Eq)
data Vector = Vector  Float Float Float               deriving (Show, Eq)
data Quaternion = Quaternion {w::Float, vec::Vector}  deriving (Show, Eq)
data Ray    = Ray     {o::Vector, dir:: Vector}       deriving (Show)
data Camera = Camera  {p::Vector, rot::Quaternion}    deriving (Show)
data Sphere = Sphere Vector Float                     deriving (Show)
  
instance Num Color where 
  (Color r1 b1 g1) + (Color r2 b2 g2) = Color (r1 + r2) (b1 + b2) (g1 + g2)  
  (*)         = undefined
  abs         = undefined
  signum      = undefined
  fromInteger = undefined
  
instance Num Vector where
  (Vector x1 y1 z1) + (Vector x2 y2 z2) = Vector (x1 + x2)  (y1 + y2) (z1 + z2)
  (Vector x1 y1 z1) - (Vector x2 y2 z2) = Vector (x1 - x2)  (y1 - y2) (z1 - z2)
  abs (Vector x y z) = undefined
  (*)         = undefined
  signum      = undefined
  fromInteger = undefined
  
vlength (Vector x y z)  = sqrt( x * x + y * y + z * z)
vtimes (Vector x y z) s = Vector ( x * s ) (y * s) (z * s)

instance Num Quaternion where
  (+) = undefined
  abs = undefined
  (Quaternion w1 (Vector x1 y1 z1)) * (Quaternion w2 (Vector x2 y2 z2)) =
    (Quaternion (w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2)
     (Vector
      (w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2)
      (w1 * y2 + y1 * w2 + z1 * x2 - x1 * z2)
      (w1 * z2 + z1 * w2 + x1 * y2 - y1 * x2)))
  signum        = undefined
  fromInteger   = undefined
  negate  (Quaternion w v) = Quaternion w (vtimes v (-1))