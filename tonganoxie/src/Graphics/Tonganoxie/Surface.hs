{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}
module Graphics.Tonganoxie.Surface where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, toList, fromList)
import qualified Data.Vector as V

import Linear.Affine (Point, (.+^))
import qualified Linear.Affine as A
import Linear.Quaternion (Quaternion)
import qualified Linear.Quaternion as Q
import Linear.V3 (V3(V3))
import Linear.V2 (V2(V2))
import Linear.Vector (liftU2)
import Linear.Metric(normalize, distance)

import System.FilePath (replaceExtension)

import Linear.Quaternion.Utils

import Graphics.Tonganoxie.Material
import Graphics.Tonganoxie.Object
import Graphics.Tonganoxie.Types

--Liia import
import Graphics.Tonganoxie.PolyTransform
import Graphics.Tonganoxie.Distance



sphere :: Surface
sphere (A.P (V2 u v))
    = A.P $ V3 (sin long * cos lat)
               (sin lat)
               (cos long * cos lat)
  where
     long = u * pi * 2     -- 0 .. 2pi
     lat  = (v - 0.5) * pi -- -pi/2 ... pi/2


plane :: Surface
plane (A.P (V2 u v)) = A.P $ V3 (f u) (f v) 0
  where
      f x = x



scaleSide :: [Double] -> [Double] -> R3
scaleSide [x, y, z] [w, h, l] = A.P $ V3 (x*w) (y*l) (z*h)


--excess
--excessSide :: [V2 Double] -> Double -> Double
--excessSide [(V2 x1 y1), (V2 x2 y2), (V2 x3 y3) (V2 x4 y4)] refDim =


--add a scale function for the vertices with respect to l w and h
side'' :: [V2 Double] -> [Double] -> R2 -> R3
side'' [(V2 x1 y1), (V2 x2 y2), (V2 x3 y3), (V2 x4 y4), (V2 x5 y5), (V2 x6 y6), (V2 x7 y7), (V2 x8 y8)] dims@[w,h,l] (A.P (V2 x y)) =
  --w goes with x, l goes with y, h goes with z
  let le = [(0,y6), (0,y2), (x2, y2), (x6, y6)]
      ls =  [(x6, y6), (x2, y2), (x1, y1), (x5, y5)]
      rs = [(x7, y7), (x3, y3), (x4, y4), (x8, y8)]
      bot = [ (x6, y6), (x5, y5), (x7, y7), (x8, y8)]
      inner = [(x5, y5), (x1, y1), (x3, y3), (x7, y7)]
      top = [ (x1, y1), (x2, y2), (x4, y4), (x3, y3)]
      re = [(x8,y8), (x4, y4), (1, y4), (1,y8)]
  in
    if (inPolygon (x, y) le) then
        let u = -1 * (distanceTo (x6, y6) (x2, y2) (x,y) ) / (distanceTo (x6, y6) (x2, y2) (0, 0) )--A.P $ V3 0 (0-x)
            v = (distanceTo (0,y6) (x6,y6) (x,y) ) / (distanceTo (0,y6) (x6, y6) (0, ((1 + y2)/2.0) ))
        in scaleSide [0, u, v] dims --A.P $ V3 (0*w) (u*l) (v*h)
      else if ( inPolygon (x,y) ls) then
        let u = (distanceTo (x6, y6) (x2, y2) (x,y) ) / (distanceTo (x6, y6) (x2, y2) (x1, y) )
            v = (distanceTo (x6, y6) (x5, y5) (x,y) ) / (distanceTo (x6, y6) (x5, y5) (x, (findY (x2, y2) (x1, y1) x)) )
          in scaleSide [0, u, v] dims --A.P $ V3 (0*w) (u*l) (v*h)
          else if (inPolygon (x,y) bot) then
            let u = (distanceTo (x6, y6) (x5, y5) (x,y) ) / (distanceTo (x6, y6) (x5, y5) ((findX (x8, y8) (x7, y7) y), y))
                v = (distanceTo (x6, y6) (x8, y8) (x,y) ) / (distanceTo (x6, y6) (x8, y8) (x, ((y5+y7)/2.0))) --(((x5 + x7)/2.0), y))
            in scaleSide [u, v, 0] dims--A.P $ V3 (u*w) (v*h) (0*l)
            else if (inPolygon (x,y) inner) then
                let u = (distanceTo (x5, y5) (x1, y1) (x,y) ) / (distanceTo (x5, y5) (x1, y1) (((x3+x7)/2.0), y))
                    v = (distanceTo (x5, y5) (x7, y7) (x,y) ) / (distanceTo (x5, y5) (x7, y7) (x, ((y1 + y3)/2.0)))
                in scaleSide [u, 1, v] dims --A.P $ V3 u 1 v
                else if (inPolygon (x,y) top) then
                  let u = (distanceTo (x2, y2) (x1, y1) (x,y) ) / (distanceTo (x2, y2) (x1, y1) ((findX (x4, y4) (x3, y3) y), y) )
                      v = (distanceTo (x2, y2) (x4, y4) (x,y) ) / (distanceTo (x2, y2) (x4, y4) (x, ((y1+y3)/2.0)))
                  in scaleSide [u, v, 1] dims --A.P $ V3 u v 1 --(u*w) (v*l) (1*h)
                  else if (inPolygon (x,y) rs) then
                      let u = (distanceTo (x8, y8) (x4, y4) (x,y) ) / (distanceTo (x8, y8) (x4, y4) (x3, y) )
                          v = (distanceTo (x8, y8) (x7, y7) (x,y) ) / (distanceTo (x8, y8) (x7, y7) (x, (findY (x4, y4) (x3, y3) x)) )
                      in scaleSide [1, u, v] dims --A.P $ V3 1 u v
                    else if (inPolygon (x,y) re) then  --A.P $ V3 w 0 (l + x)
                      let u = -1 * (distanceTo (x8, y8) (x4, y4) (x,y) ) / (distanceTo (x8, y8) (x4, y4) (1, ((y4 + y8)/2.0) ))
                          v = (distanceTo (0,0) (1, 0) (x,y) ) / (distanceTo (0,0) (1, 0) (0,1) )
                      in  scaleSide [1, u, v] dims --A.P $ V3 1 u v --(1*w) (u*l) (v*h)
                      else A.P $ V3 7 7 7
