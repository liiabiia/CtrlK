{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}
module Graphics.Tonganoxie.SurfaceHelpers where

import Graphics.Tonganoxie.Material
import Graphics.Tonganoxie.Object
import Graphics.Tonganoxie.Types
import Graphics.Tonganoxie.PolyTransform
import Graphics.Tonganoxie.Distance

import Linear.Quaternion.Utils
import qualified Linear.Affine as A
import Linear.V3 (V3(V3))
import Linear.V2 (V2(V2))

propFind :: Double -> Double -> Double
propFind a b = if (b /= 0) then a/b else 1

scaleSide :: [Double] -> [Double] -> R3
scaleSide [x, y, z] [w, h, l] = A.P $ V3 (x*w) (y*l) (z*h)

--can generalize with min
calcArea :: [(Double, Double)] -> Double
calcArea shape@[(x0,y0), (x1,y1), (x2, y2), (x3, y3)] = a
  where
    a1 = ((distancePoints x0 x3) * (distancePoints y0 y3))/2.0
    a2 = ((distancePoints x1 x2) * (distancePoints y1 y2))/2.0
    a = (a1 + a2)/2.0

lXMap :: [(Double, Double)] -> (Double, Double) -> Double
lXMap shape@[(x0,y0), (x1,y1), (x2, y2), (x3, y3)] (x, y) = xPt
  where
    tri1X = ((distancePoints x0 x) * (distancePoints y0 (findY (x0, y0) (x3, y3) x)))
    tri1Tot = (distancePoints x0 x3) * (distancePoints y0 y3)
    prop1 = propFind tri1X  tri1Tot
    tri2X = (distancePoints x1 x) * (distancePoints y1 (findY (x1, y1) (x2, y2) x))
    tri2Tot = (distancePoints x1 x2) * (distancePoints y1 y2)
    prop2 = propFind tri2X  tri2Tot
    xPt = (prop1 + prop2) / 2.0

rXMap :: [(Double, Double)] -> (Double, Double) -> Double
rXMap shape@[(x0,y0), (x1,y1), (x2, y2), (x3, y3)] (x, y) = xPt
  where
    tri1X = ((distancePoints x3 x) * (distancePoints y3 (findY (x0, y0) (x3, y3) x)))
    tri1Tot = (distancePoints x0 x3) * (distancePoints y0 y3)
    prop1 = propFind tri1X tri1Tot
    tri2X = (distancePoints x2 x) * (distancePoints y2 (findY (x1, y1) (x2, y2) x))
    tri2Tot = (distancePoints x1 x2) * (distancePoints y1 y2)
    prop2 = propFind tri2X tri2Tot
    xPt = (prop1 + prop2) / 2.0

tYMap :: [(Double, Double)] -> (Double, Double) -> Double
tYMap shape@[(x0,y0), (x1,y1), (x2, y2), (x3, y3)] (x, y) = yPt
  where
    tri1Y = ((distancePoints y0 y) * (distancePoints x0 (findX (x0, y0) (x1, y1) y)))
    tri1Tot = ((distancePoints y0 y1) * (distancePoints x0 x1))
    prop1 = propFind tri1Y tri1Tot
    tri2Y = ((distancePoints y3 y) * (distancePoints x3 (findX (x2, y2) (x3, y3) y)))
    tri2Tot = ((distancePoints y3 y2) * (distancePoints x3 x2))
    prop2 = propFind tri2Y tri2Tot
    yPt = (prop1 + prop2) / 2.0

bYMap :: [(Double, Double)] -> (Double, Double) -> Double
bYMap shape@[(x0,y0), (x1,y1), (x2, y2), (x3, y3)] (x, y) = yPt
  where
    tri1Y = ((distancePoints y0 y) * (distancePoints x0 (findX (x0, y0) (x1,y1) y)))
    tri1Tot = ((distancePoints y0 y1)) * ((distancePoints x0 x1))
    prop1 = tri1Y / tri1Tot
    tri2Y = ((distancePoints y3 y) * (distancePoints x3 (findX (x2, y2) (x3, y3) y)))
    tri2Tot = ((distancePoints y3 y2) * (distancePoints x3 x2))
    prop2 = propFind tri2Y  tri2Tot
    yPt = (prop1 + prop2) / 2.0


topEdge :: [(Double, Double)] -> [(Double, Double)] -> (Double, Double) -> (Double, Double) -> (Double, Double) -> [Double] -> R3
topEdge top te@[(xTL, yTL), (x2, y2), (x4, y4), (xTR, yTR)] (x,y) lLeft@(xL, yL) lRight@(xR, yR) dims@[w, h, l] = pt
  where
    te'@[(xTL', yTL'), (x2', y2'), (x4', y4'), (xTR', yTR')] = excessTop te lLeft lRight
    mapTri = [(xTL', yTL'), (xL, yL), (xR, yR), (xTR', yTR')]
    lInv = bYMap te' (x,y)
    w = (distancePoints xTL' x2') + (distancePoints xTR' x4') / 2.0
    h = (distancePoints yTL' y2') + (distancePoints yTR' y4') /2.0
    prop = if (w /= 0) then (h*h) / w else 0
    u = (distanceTo (xTL', yTL') (x2', y2') (x,y) ) / (distanceTo (xTL', yTL') (x2', y2') ((findX (xTR', yTR') (x4', y4') y), y))
    v = -1 * prop * lInv -- -1 * l' * (distanceTo (x2', y2') (x4', y4') (x,y) ) / (distanceTo (x2', y2') (x4', y4') (x, ((yTL'+yTR')/2.0)))
    pt = scaleSide [u, v, 1] dims

--excess top
excessTop :: [(Double, Double)] -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
excessTop te@[(xTL, yTL), (x2, y2), (x4, y4), (xTR, yTR)] lLeft@(xL, yL) lRight@(xR, yR) = totTop
  where
    (xTL', yTL') = ((findX (xTL, yTL) (x2, y2) yTL), yTL)
    (xTR', yTR') = ((findX (xTR, yTR) (x4, y4) yTR), yTR)
    totTop = [(xTL', yTL'), (x2, y2), (x4, y4), (xTR', yTR')]


--botEdge
botEdge ::[(Double, Double)] -> [(Double, Double)] -> (Double, Double) -> (Double, Double) -> (Double, Double) -> [Double] -> R3
botEdge bot be@[(xBL, yBL), (x6, y6), (x8, y8), (xBR, yBR)] (x,y) lLeft@(xL, yL) lRight@(xR, yR) dims@[w,h,l] = pt
  where
    be'@[(xBL', yBL'), (x6', y6'), (x8', y8'), (xBR', yBR')] = excessBottom be lLeft lRight
    mapTri = [(xBL', yBL'),(xL, yL),(xR, yR),(xBR, yBR)]
    lInv = tYMap be' (x,y)
    w = (distancePoints xBL' x6') + (distancePoints xBR' x8') /2.0
    h = (distancePoints yBL' y6') + (distancePoints yBR' y8') /2.0
    prop = if (w /= 0) then (h*h)/w else 0
    u = (distanceTo (xBL', yBL') (x6', y6') (x,y) ) / (distanceTo (xBL', yBL') (x6', y6') ((findX (xBR', yBR') (x8', y8') y), y))
    v = -1 * prop * lInv-- -1*l'*(distanceTo (x6', y6') (x8', y8') (x,y) ) / (distanceTo (x6', y6') (x8', y8') (x, ((yBL'+yBR')/2.0)))
    pt = scaleSide [u, v, 0] dims

--excess bottom
excessBottom :: [(Double, Double)] -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
excessBottom be@[(xBL, yBL), (x6, y6), (x8, y8), (xBR, yBR)] lLeft@(xL, yL) lRight@(xR, yR) = totBot
  where
    (xBL', yBL') = ((findX (xBL, yBL) (x6, y6) yBL), yBL)
    (xBR', yBR') = ((findX (xBR, yBR) (x8, y8) yBR), yBL)
    totBot = [(xBL', yBL'), (x6, y6), (x8, y8), (xBR', yBR')]

--leftEdge
leftEdge :: [(Double, Double)] -> [(Double, Double)] -> (Double, Double) -> (Double, Double) -> (Double, Double) -> [Double] -> R3
leftEdge ls le@[(x0, y0), (x1, y1), (x2, y2), (x6, y6)] (x, y) lTop@(xT, yT) lBot@(xB, yB) dims@[w, h, l] = pt
  where
    le'@[(x0', y0'), (x1', y1'), (x2', y2'), (x6', y6')] = excessLeft le lBot lTop
    mapTri = [(x0', y0'), (x1', y1'), (xT, yT), (xB, yB)]
    lInv = rXMap le' (x,y)
    edgeA = calcArea le'
    notEdge = ((calcArea mapTri) - (calcArea le'))
    forLength = ((distancePoints xT x2') + (distancePoints xB x6'))
    w = (distancePoints x0' x6') + (distancePoints x1' x2' ) / 2.0
    h = (distancePoints y0' y6') + (distancePoints y1' y2') / 2.0
    prop = if (h /= 0) then (w*w)/h else w/forLength--(distancePoints ((x0'+x1')/2.0) ((x2' + x6') / 2.0)) / (distancePoints ((x0'+x1')/2.0) ((x2' + x6')/2.0) )--edgeA / notEdge
    u = -1 * prop * (lInv)
    v = (distanceTo (x0',y0') (x6',y6') (x,y) ) / (distanceTo (x0',y0') (x6', y6') (x, (findY (x1', y1') (x2', y2') x)))
    pt = scaleSide [0, u, v] dims


--excess left
excessLeft :: [(Double, Double)] -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
excessLeft [(x0, y0), (x1, y1), (x2, y2), (x6, y6)] lBot@(xB, yB) lTop@(xT, yT) = totLE
  where
    (x0', y0') = (x0, (findY (xB, yB) (x6, y6) x0))
    (x1', y1') = (x1, (findY (xT, yT) (x2, y2) x1))
    totLE = [(x0', y0'), (x1', y1'), (x2, y2), (x6, y6)]


rightEdge :: [(Double, Double)] -> [(Double, Double)] -> (Double, Double) -> (Double, Double) -> (Double, Double) -> [Double] -> R3
rightEdge rs re@[(x8, y8), (x4, y4), (x1, y1), (x0, y0)] (x, y) lTop@(xT, yT) lBot@(xB, yB) dims@[w, h, l] = pt
      where
        re'@[(x8', y8'), (x4', y4'), (x1', y1'), (x0', y0')] = excessRight re lBot lTop
        mapTri = [(xB,yB), (xT, yT), (x1', y1'), (x0', y0')]
        lInv = lXMap re' (x,y)
        edgeA = calcArea re'
        notEdge = ((calcArea mapTri) - calcArea re')
        forLength = ((distancePoints xT x4') + (distancePoints xB x8'))
        w = (distancePoints x8' x0' ) + (distancePoints x4' x1' ) / 2.0
        h = (distancePoints y8' y0') + (distancePoints y4' y1' ) / 2.0
        prop = if (h /= 0) then (w*w)/h else (w / forLength) --edgeA / notEdge
        u = -1 * prop * lInv
        v = (distanceTo (x8', y8') (x0', y0') (x,y) ) / (distanceTo (x8', y8') (x0', y0') (x, (findY (x4', y4') (x1', y1') x)) )
        pt = scaleSide [1, u, v] dims

--excess right
excessRight :: [(Double, Double)] -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
excessRight [(x8, y8), (x4, y4), (x1, y1), (x0, y0)] lBot@(xB, yB) lTop@(xT, yT) = totRE
  where
    (x1', y1') = (x1, (findY (xT, yT) (x4, y4) x1))
    (x0', y0') = (x0, (findY (xB, yB) (x8, y8) x0))
    totRE = [(x8, y8), (x4, y4), (x1', y1'), (x0', y0')]
