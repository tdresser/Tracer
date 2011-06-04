import qualified Data.ByteString as B
import Data.Maybe
import Tracertypes
import Tga
import Debug.Trace

tolerance = 0.001

red :: Int -> B.ByteString
red n = B.pack $ foldr (++) [] $ take n $ repeat $ getAsByteArray (Color 1 0 0)

camera = Camera (Vector 0 0 0) $ qnorm $ Quaternion 1 $ Vector 0 0 0
width = 100
height = 100
dist = 100

vzero = Vector 0 0 0

vtimes :: Vector -> Float -> Vector
vtimes (Vector x y z) s = 
  Vector ( x * s ) (y * s) (z * s)

vlength (Vector x y z) = 
  sqrt( x * x + y * y + z * z)
  
vdist x y = 
  vlength $ vminus x y

vnorm v@(Vector x y z) = 
  Vector (x / d) (y / d) (z / d)
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
  Vector (x1 - x2)  (y1 - y2) (z1 - z2)
  
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) = 
  Vector (x1 + x2)  (y1 + y2) (z1 + z2)

rotate v q =
  (vec 
    (qmultq 
     (qmultq qn (Quaternion 0 v)) 
     (qconj qn)))
   where qn = qnorm q
        
v = Vector 1 0 0
q = Quaternion 2 $ Vector 3 4 5

getRay (Camera p q) (x,y) = 
  rayfromto p $ rotate (Vector x y dist) q

rays = [getRay camera (x,y) | 
        x<- [ (-fwidth)  / 2 .. (fwidth)  / 2 - 1], 
        y<- [ (-fheight) / 2 .. (fheight) / 2 - 1]]
  where
    fwidth = fromIntegral width
    fheight = fromIntegral height


rayfromto o p = Ray o $ vnorm $ vminus p o

s = Sphere (Vector 0 0 100) 40
light = Vector 0 70 40

raysphere (Ray o l) (Sphere c r) =
  let 
      tc = vminus c o
      sroot = vdot l tc * vdot l tc - vdot tc tc + r * r
      s = sqrt $ sroot
      nonroot = vdot l tc
      d1 = nonroot - s
      d2 = nonroot + s in
  case () of _
               | sroot <= 0        -> Nothing
               | d1 > tolerance    -> Just $ vplus o $ vtimes l d1
               | d2 > tolerance    -> Just $ vplus o $ vtimes l d2
               | otherwise         -> Nothing

distanceFromCameraToColor vec = 
  let 
    c = vdist (p camera) vec in
    Color (c / 100) (c / 100) (c / 100)

shadePointOnObject p =
  --distanceFromCameraToColor $ p
  let i = raysphere (rayfromto p light) s in
  if isJust $ i then
    Color 0.5 0 0
  else
    Color 1 0 0

raycolor r s =
  if isJust $ i then
    getAsByteArray $ shadePointOnObject $ fromMaybe vzero i
  else
    getAsByteArray (Color 0 0 0)
      where i = raysphere r s

getSphereIntersection r = 
  raysphere r s
  
getLightIntersection r = 
  raysphere (rayfromto (fromMaybe vzero (raysphere r s)) light) s

getIntersections r = 
  [getSphereIntersection r, getLightIntersection r]

colors = map (\r -> raycolor r s) rays
image = B.pack $ foldr (++) [] $ colors

main = do 
  B.writeFile "test.tga" $ B.append (tgaHeader width height) 
    image
    
-- *Main Data.Maybe> raysphere r s
-- Just (Vector 0.0 0.0 46.054688)
-- *Main Data.Maybe> r
-- Ray (Vector 0.0 0.0 71.0) (Vector 0.0 0.0 60.0)
-- *Main Data.Maybe> s
-- Sphere (Vector 0.0 0.0 101.0) 30.0