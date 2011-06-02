import qualified Data.ByteString as B
import Tracertypes
import Tga

red :: Int -> B.ByteString
red n = B.pack $ foldr (++) [] $ take n $ repeat $ getAsByteArray (Color 1 0 0)

camera = Camera (Vector 0 0 0) (Quaternion 1 (Vector 0 0 0))
width = 100
height = 100
dist = 100

vtimes :: Vector -> Float -> Vector
vtimes (Vector x y z) s = 
  Vector ( x * s ) (y * s) (z * s)

vlength (Vector x y z) = 
  sqrt( x * x + y * y + z * z)

vnorm v@(Vector x y z) = 
  Vector (x / d) (y / 2) (z / d)
    where d = vlength v

qnorm q@(Quaternion w (Vector x y z)) =
  Quaternion (w / d) (Vector (x / d) (y / d) (z / d))
    where d = qlength q
          
qlength (Quaternion w (Vector x y z)) =
  sqrt(w * w + x * x + y * y + z * z)

qconj (Quaternion w v) = 
  Quaternion w (vtimes v (-1))

qmultq (Quaternion w1 (Vector x1 y1 z1)) (Quaternion w2 (Vector x2 y2 z2)) =
  (Quaternion (w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2)
   (Vector
    (w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2)
    (w1 * y2 + y1 * w2 + z1 * x2 - x1 * z2)
    (w1 * z2 + z1 * w2 + x1 * y2 - y1 * x2)))

vdot (Vector x1 y1 z1) (Vector x2 y2 z2) = 
  x1 * x2 + y1 * y2 + z1 * z2

vminus (Vector x1 y1 z1) (Vector x2 y2 z2) = 
  Vector (x1 - x2)  (y1 - y2) (z1 * z2)

rotate v q =
  (vec 
    (qmultq 
     (qmultq qn (Quaternion 0 v)) 
     (qconj qn)))
   where qn = qnorm q
        
v = Vector 1 0 0
q = Quaternion 2 $ Vector 3 4 5

getRay (Camera p q) (x,y) = 
  Ray p $ vnorm $ rotate (Vector x y dist) q

rays = [getRay camera (x,y) | 
        x<- [ (-fwidth)  / 2 .. (fwidth)  / 2 - 1], 
        y<- [ (-fheight) / 2 .. (fheight) / 2 - 1]]
  where
    fwidth = fromIntegral width
    fheight = fromIntegral height

s = Sphere (Vector 0 0 101) 30

raysphere (Ray o l) (Sphere c r) =
  vdot l c * vdot l c - vdot c c + r * r >= 0


raycolor r s = 
  if raysphere r s then
    getAsByteArray (Color 1 0 0)
  else
    getAsByteArray (Color 0 0 0)

colors = map (\r -> raycolor r s) rays
image = B.pack $ foldr (++) [] $ colors

main = do 
  B.writeFile "test.tga" $ B.append (tgaHeader width height) 
    image