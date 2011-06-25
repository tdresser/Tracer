import qualified Data.ByteString as B
import Data.Maybe
import Tracertypes
import Tga
import Debug.Trace

tolerance = 0.001
ambient = 0.1

red :: Int -> B.ByteString
red n = B.pack $ foldr (++) [] $ take n $ repeat $ getAsByteArray (Color 1 0 0)

camera = Camera (Vector 0 0 0) $ qnorm $ Quaternion 1 $ Vector 0 0 0
width = 1024
height = 768
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
        y<- [ (-fheight)  / 2 .. (fheight)  / 2 - 1], 
        x<- [ (-fwidth) / 2 .. (fwidth) / 2 - 1]]
  where
    fwidth = fromIntegral width
    fheight = fromIntegral height


rayfromto o p = Ray o $ vnorm $ p - o

shapes = [
  Shape (Sphere (Vector 30 (-60) 90) 60) (Shader (Color 0.5 0 0)),
  Shape (Sphere (Vector 0 60 100) 60) (Shader (Color 0 0.5 0)),
  Shape (Sphere (Vector 100 60 150) 60) (Shader (Color 0 0.2 0.5)) ]

lights = [Vector 20 40 (10)]
--  ,Vector 20 (-40) (10)]

normal (Sphere c r) p =
  vnorm $ p - c

rayIntersectionWithShape :: Ray -> Shape -> Maybe Intersection
rayIntersectionWithShape (Ray o l) shape@(Shape (Sphere c r) _)=
  let 
      tc = c - o
      sroot = vdot l tc * vdot l tc - vdot tc tc + r * r
      s = sqrt $ sroot
      nonroot = vdot l tc
      d1 = nonroot - s
      d2 = nonroot + s in
  case () of _
               | sroot <= 0        -> Nothing
               | d1 > tolerance    -> Just $ 
                                      Intersection (o + vtimes l d1) shape
               | d2 > tolerance    -> Just $ 
                                      Intersection (o + vtimes l d2) shape
               | otherwise         -> Nothing
  
-- compares two Maybe intersections, and returns the closer one
closestIntersection :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
closestIntersection Nothing a = a
closestIntersection a Nothing = a
closestIntersection a@(Just (Intersection avec _)) b@(Just ( Intersection bvec _)) = 
                 if   vlength ( avec - (p camera)) 
                    < vlength ( bvec - (p camera)) then
                   a
                 else
                   b

-- finds the closest intersection point, and the shape intersected
rayIntersection r shapes =
  foldr (closestIntersection . rayIntersectionWithShape r) Nothing shapes

ctimes (Color r g b) t =
  Color ( r * t ) ( g * t ) ( b * t)

shadePointOnObjectForLight :: Vector -> Shape -> Vector -> Color
shadePointOnObjectForLight p (Shape o (Shader c)) light =
  let lightray = (rayfromto p light)
      shadowCaster = rayIntersection lightray shapes in
  case shadowCaster of
    -- no ray from object to light
    Just (Intersection i o) -> (Color 0 0 0)
    -- nothing between object and light
    Nothing -> 
      let intensity = vdot (vnorm (normal o p)) (vnorm (dir lightray)) in
      ctimes c intensity

ambientForShape (Shape _ (Shader (Color r g b))) = 
  (Color (r * ambient) (g * ambient) (b * ambient))

reflectionColor (Ray o dir) intersectionPoint iterationNumber normal = 
  rayColorHelper (Ray intersectionPoint (dir - (vtimes normal ( 2 * (vdot dir normal))))) (iterationNumber + 1)

rayColor r = 
  rayColorHelper r 0

depth = 10

phongPointOnObjectForLight point (Shape sphere _) light = 
  let h = vnorm (light - point) in
  ctimes (Color 1 1 1) $ (1 / vlength (point - light) ^^ 2) * 1000 * ((vdot (normal sphere point) h) ^^ 10)

rayColorHelper r iterationNumber = 
  if (iterationNumber > depth) then
    (Color 0 0 0)
  else
    case intersection of 
      -- ray hits object
      Just (Intersection point shape@(Shape sphere _)) -> 
        ambientForShape shape + 
        foldr ((+).shadePointOnObjectForLight point shape) (Color 0 0 0) lights + 
        ctimes (reflectionColor r point iterationNumber (normal sphere point )) 0.7 +
        foldr ((+).phongPointOnObjectForLight point shape) (Color 0 0 0) lights
      -- ray hits empty space
      Nothing -> (Color 0 0 0)
    where intersection = rayIntersection r shapes
            
e = 2.7182818
exposure = -1

exposureCorrect (Color r g b) = 
  (Color 
   (1 - e ** (r * exposure))
   (1 - e ** (g * exposure))
   (1 - e ** (b * exposure)))

colors = map (getAsByteArray.exposureCorrect.rayColor) rays
image = B.pack $ foldr (++) [] $ colors

main = do 
  B.writeFile "test.tga" $ B.append (tgaHeader width height) 
    image
    
--DEBUG
distanceFromCameraToColor vec = 
  let 
    c = vdist (p camera) vec in
    Color (c / 100) (c / 100) (c / 100)