import qualified Data.ByteString as B
import Data.Maybe
import Tracertypes
import Tga
import Debug.Trace

tolerance = 0.001
ambient = (Color 0.2 0 0)

red :: Int -> B.ByteString
red n = B.pack $ foldr (++) [] $ take n $ repeat $ getAsByteArray (Color 1 0 0)

camera = Camera (Vector 0 0 0) $ qnorm $ Quaternion 1 $ Vector 0 0 0
width = 100
height = 100
dist = 100

vzero = Vector 0 0 0

vdist x y = 
  vlength $ x - y

vnorm v@(Vector x y z) = 
  Vector (x / d) (y / d) (z / d)
    where d = vlength v
          
qnorm q@(Quaternion w (Vector x y z)) =
  Quaternion (w / d) (Vector (x / d) (y / d) (z / d))
    where d = qlength q
          
qlength (Quaternion w (Vector x y z)) =
  sqrt(w * w + x * x + y * y + z * z)

vdot (Vector x1 y1 z1) (Vector x2 y2 z2) = 
  x1 * x2 + y1 * y2 + z1 * z2

rotate v q =
  vec $ qn * (Quaternion 0 v) * (-qn)
   where qn = qnorm q
        
v = Vector 1 0 0
q = Quaternion 2 $ Vector 3 4 5

getRayForPixel (Camera p q) (x,y) = 
  rayfromto p $ rotate (Vector x y dist) q

rays = [getRayForPixel camera (x,y) | 
        x<- [ (-fwidth)  / 2 .. (fwidth)  / 2 - 1], 
        y<- [ (-fheight) / 2 .. (fheight) / 2 - 1]]
  where
    fwidth = fromIntegral width
    fheight = fromIntegral height


rayfromto o p = Ray o $ vnorm $ p - o

shapes = [
  Sphere (Vector 0 0 100) 40,
  Sphere (Vector 0 0 100) 40 ]

s = shapes !! 0

light = Vector 0 0 50

normal (Sphere c r) p =
  rayfromto c p

rayIntersection (Ray o l) (Sphere c r) =
  let 
      tc = c - o
      sroot = vdot l tc * vdot l tc - vdot tc tc + r * r
      s = sqrt $ sroot
      nonroot = vdot l tc
      d1 = nonroot - s
      d2 = nonroot + s in
  case () of _
               | sroot <= 0        -> Nothing
               | d1 > tolerance    -> Just $ o + vtimes l d1
               | d2 > tolerance    -> Just $ o + vtimes l d2
               | otherwise         -> Nothing

ctimes (Color r g b) t =
  Color ( r * t ) ( g * t ) ( b * t)

shadePointOnObject p o =
  let lightray = (rayfromto p light)
      imaybe = rayIntersection lightray o 
      i = fromMaybe vzero imaybe in
  if isJust $ imaybe then
    ambient
  else
    let intensity = 1 - vdot (-(dir $ normal o i)) (dir lightray) in
    ambient + ctimes (Color 0.7 0 0) intensity

rayColorFromShape r s = 
  if isJust $ i then
    getAsByteArray $ shadePointOnObject (fromMaybe vzero i) s
  else
    getAsByteArray (Color 0 0 0)
      where i = rayIntersection r s
            
raycolor r =
  rayColorFromShape r s

colors = map raycolor rays
image = B.pack $ foldr (++) [] $ colors

main = do 
  B.writeFile "test.tga" $ B.append (tgaHeader width height) 
    image
    
--DEBUG
distanceFromCameraToColor vec = 
  let 
    c = vdist (p camera) vec in
    Color (c / 100) (c / 100) (c / 100)
    
-- getSphereIntersection r = 
--   rayIntersection r s
  
-- getLightIntersection r = 
--   rayIntersection (rayfromto (fromMaybe vzero (rayIntersection r s)) light) s
  
-- getIntersections r = 
--   [getSphereIntersection r, getLightIntersection r]