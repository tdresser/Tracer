module Tracertypes (
  Color(Color), 
  Vector(Vector), 
  Ray(Ray), 
  Camera(..), 
  Quaternion(..),
  Sphere(Sphere)) where

data Color  = Color   {r::Float, g::Float, b::Float}  deriving (Show)
data Vector = Vector  Float Float Float               deriving (Show)
data Quaternion = Quaternion {w::Float, vec::Vector}  deriving (Show)
data Ray    = Ray     Vector Vector                   deriving (Show)
data Camera = Camera  {p::Vector, rot::Quaternion}     deriving (Show)
data Sphere = Sphere Vector Float                     deriving (Show)