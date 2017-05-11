{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}

--adapted from https://rosettacode.org/wiki/Ray-casting_algorithm

module Graphics.Tonganoxie.PolyTransform where

import Data.Ratio
import Linear.Epsilon

type Point' = (Double, Double)
type Polygon' = [Point']
type Line = (Double, Double)
type LineSegment = ((Double, Double), (Double, Double))


round5' :: Double -> Int
round5' x = truncate (x * 10^5)

round5 :: Double -> Double
--round5 x = x * (10.0^^5)
round5 x = (fromInteger $ truncate $ x * (10^5)) / (10.0^^5)

xIntercept :: Line -> Double
xIntercept (m, b) = ((-1*b) / m)



polySides :: Polygon' -> [(Point', Point')]
polySides poly@(p1:ps) = zip poly $ ps ++ [p1]

intersects :: Point' -> Line -> Bool
intersects (px, py) (m, b) =
    if (m < 0) then (round5 py) <= (round5 (m * px + b)) else (round5 py) >= (round5 (m *px + b))

onLine :: Point' -> LineSegment -> Bool
onLine (px, py) ((x1,y1), (x2, y2)) = if(round5' (x2-x1)==0) then (round5' (x2-px)) == (round5' (x2-x1)) else (round5' py) == (round5' (m* px + b))
  where
    (m, b) = makeLine ((x1,y1), (x2, y2))

makeLine :: (Point', Point') -> Line
makeLine ((x1, y1), (x2, y2)) = (slope, yintercept)
  where
        slope = (y1 - y2) /  (x1 - x2)
        yintercept = y1 - slope * x1

between :: Ord a => a -> a -> a -> Bool
between x a b =
  if(a > b) then (b <= x && x <= a)
  else (a <= x && x <= b)

{-inPolyInt :: Point' -> Polygon' -> Int
inPolyInt p@(px,py) = f 0 . polySides
  where f n [] = n
        f n (side : sides) | far = f (n+1) sides
                           | onSegment = n
                           | rayIntersects = (n+1) --f (n + 1) sides
                           | otherwise = f (n+1) sides
                           where
                             far = not $ between py ay by
                             onSegment | ay == by = between px ax bx
                                       | otherwise = p `onLine` line
                             rayIntersects =
                               intersects p line &&
                               (py /= ay || by < py) &&
                               (py /= by || ay < py)
                             ((ax, ay), (bx, by)) = side
                             line = makeLine side-}


inPoly :: Polygon' -> Point' -> Bool
inPoly pl (x,y) = inPolygon pt' pl'
  where
    pt' = ((round5 x), (round5 y))
    pl' = map (\ (x, y) -> ((round5 x), (round5 y))) pl

inPolygon :: Point' -> Polygon' -> Bool
inPolygon p@(px, py) pl = f 0 (polySides pl)
  where f n [] = odd n
        f n (side : sides) = if (thePoint) then True else if (far || above || below) then f n sides else if (onSegment) then True else if (right) then f n sides else if (rayIntersects) then f (n+1) sides else f n sides
            where
              thePoint =  ((round5' px) == (round5' ax) && (round5' py) == (round5' ay)) || ((round5' px) == (round5' bx) && (round5' py) == (round5' by))
              far = not $ between py ay by
              right = px > max ax bx
              above = py > max ay by
              below = py < min ay by
              onSegment = if (ay == by) then between px ax bx else p `onLine` ((ax,ay), (bx,by))
              rayIntersects = if (far || right) then False else if (below) then True else intersects p line && (py /= ay || by < py) && (py /= by || ay < py)
              ((ax, ay), (bx, by)) = side
              line = makeLine side
