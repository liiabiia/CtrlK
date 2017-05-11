{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings, DeriveFunctor #-}

module Graphics.Tonganoxie.Tessellation where

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
import Linear.V3 (V3(V3), cross)
import Linear.V2 (V2(V2))
import Linear.V4 (V4(V4))
import Linear.Vector (liftU2)
import Linear.Metric(normalize, distance)

import System.FilePath (replaceExtension)

import Linear.Quaternion.Utils

import Graphics.Tonganoxie.Material
-- import Graphics.Tonganoxie.Object hiding (points, faces)
import Graphics.Tonganoxie.Types as T

import Graphics.Tonganoxie.PolyTransform as P
import Graphics.Tonganoxie.Distance


-- | A 'Mesh' is a list of points, and a list of faces.
data Mesh g p = Mesh
 { points :: Vector p
 , faces  :: [g PT]
 } deriving (Functor)

instance (Show p, Polygon g) => Show (Mesh g p) where
    show (Mesh p f) = show (p,map vertices f)


dtoV2 :: [(Double, Double)] -> [V2 Double]
dtoV2 ls = map (\(x,y) -> V2 x y) ls

createRPts :: [(Double,Double)] -> [R2]
createRPts ls = map (\ (x,y) -> A.P $ V2 x y) ls

v2ToR2 :: [V2 Double] -> [R2]
v2ToR2 ls = map (\ x -> A.P $ x) ls

--convert to vector after

createPtLst :: [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)]
createPtLst ls [(x1, y1), (x2, y2), (x3, y3), (x4, y4)] = filter (P.inPoly [(x1, y1), (x2, y2), (x3, y3), (x4, y4)]) ls

whichParts :: [[(Double, Double)]] -> [[(Double, Double)]]
whichParts ls = filter (shouldInclude) ls


createFs :: Int -> Int -> ([V3 PT], Int)
createFs ptSize prevSize = (faces, nextSize)
  where
    faces = fmap (fmap ix)
         [ f
         | (u,u') <- us `zip` tail us
         , (v,v') <- vs `zip` tail vs
         , f <- [ V3 (u,v) (u,v') (u',v')
                , V3 (u',v') (u',v) (u,v)
                ]
         ]
    ix (u,v) = PT $ u + v * (ptSize + 1)
    us = [prevSize..ptSize+prevSize-1] :: [Int]
    vs = [prevSize..ptSize+prevSize-1] :: [Int]
    nextSize = prevSize + (ptSize + prevSize)*(ptSize +1)

createFcs :: Int -> Int -> [V3 PT]
createFcs num offset = faces
  where
    faces = fmap (fmap ix)
          [ f
          | (u,u') <- us `zip` tail us
          , (v,v') <- vs `zip` tail vs
          , f <- [ V3 (u,v) (u,v') (u',v')
                 , V3 (u',v') (u',v) (u,v)
                 ]
          ]
    ix (u, v) = PT $ (offset *num * num) + u + v * (num)
    us = [0..num-1] --0 indexing
    vs = [0..num-1]


--create faces from start index to end index (going to be the same range each time) plus offset
createFcs' :: [[(Double, Double)]] -> Int -> [V3 PT]
createFcs' ls num = faces'
  where
    faces' = concat faces
    faces = map ( \ x -> createFcs num x) x'
    x' = [0..((length ls)-1)] :: [Int]


createPts :: [V2 Double] -> Int -> [R2]
createPts [(V2 x1 y1), (V2 x2 y2), (V2 x3 y3), (V2 x4 y4), (V2 x5 y5), (V2 x6 y6), (V2 x7 y7), (V2 x8 y8)] num = points
  where
    le = [(0,y6), (0,y2), (x2, y2), (x6, y6)]
    ls =  [(x6, y6), (x2, y2), (x1, y1), (x5, y5)]
    bot = [ (x6, y6), (x5, y5), (x7, y7), (x8, y8)]
    inner = [(x5, y5), (x1, y1), (x3, y3), (x7, y7)]
    top = [ (x2, y2), (x1, y1), (x3, y3), (x4, y4)]
    rs = [(x7, y7), (x3, y3), (x4, y4), (x8, y8)]
    re = [(x8,y8), (x4, y4), (1, y4), (1,y8)]
    be = [(x6,0), (x6,y6), (x8, y8), (x8,0)]
    te = [(x2,1), (x2, y2), (x4, y4), (x4, 1)]
    blcnr = [(0,0), (0,y6), (x6, y6), (x6,0)]
    tlcnr = [(0, y2), (0,1), (x2, 1), (x2,y2)]
    brcnr = [(x8,0), (x8,y8), (1, y8), (1,0) ]
    trcnr = [(x4, y4), (x4,1), (1,1), (1,y4)]
    everything = [blcnr, le, tlcnr, ls, be, bot, inner, top, te, rs, brcnr, re, trcnr]
    justmin = [ls, bot, inner, top, rs]
    includeEdges = [le, ls, be, bot, inner, top, te, rs, re]
    includePoints = whichParts includeEdges


    points =  concat (map (\ x -> createPts' x num ) includePoints)

createPts'' :: [(Double, Double)] -> [(Double, Double)] -> Int -> [R2]
createPts'' l1 l2 num = points
  where
    points = createRPts (concat calcPoints)
    calcPoints = map ( \ ((x,y), (x',y')) -> findPointsBetween num (x, y) (x', y')) $ zip l1 l2

createPts'::[(Double, Double)] -> Int -> [R2]
createPts' [(x1, y1), (x2, y2), (x3, y3), (x4, y4)] num =
  let l1 = findPointsBetween num (x1, y1) (x4, y4)
      l2 = findPointsBetween num (x2, y2) (x3, y3)
  in createPts'' l1 l2 num


--Add filter for which areas to include if something goes to zero

createTess :: [V2 Double] -> Int -> Mesh V3 R2
createTess thePoints@[(V2 x1 y1), (V2 x2 y2), (V2 x3 y3), (V2 x4 y4), (V2 x5 y5), (V2 x6 y6), (V2 x7 y7), (V2 x8 y8)] num = Mesh points' faces
  where
    le = [(0,y6), (0,y2), (x2, y2), (x6, y6)]
    ls =  [(x2, y2), (x1, y1), (x5, y5), (x6, y6)]
    rs = [(x7, y7), (x3, y3), (x4, y4), (x8, y8)]
    bot = [ (x5, y5), (x7, y7), (x8, y8), (x6, y6)]
    inner = [(x1, y1), (x3, y3), (x7, y7), (x5, y5)]
    top = [ (x2, y2), (x1, y1), (x3, y3), (x4, y4)]
    re = [(x8,y8), (x4, y4), (1, y4), (1,y8)]
    be = [(x6,0), (x6,y6), (x8, y8), (x8,0)]
    te = [(x2,1), (x2, y2), (x4, y4), (x4, 1)]
    blcnr = [(0,0), (0,y6), (x6, y6), (x6,0)]
    tlcnr = [(0, y2), (0,1), (x2, 1), (x2,y2)]
    brcnr = [(x8,0), (x8,y8), (1, y8), (1,0) ]
    trcnr = [(x4, y4), (x4,1), (1,1), (1,y4)]
    maxImage = [blcnr, le, tlcnr, ls, be, bot, inner, top, te, rs, brcnr, re, trcnr]
    justmin = [ls, bot, inner, top, rs]
    includeEdges = [le, ls, be, bot, inner, top, te, rs, re]
    points = createPts thePoints num
    faces = createFcs' (whichParts includeEdges) num
    points' = V.fromList (points)



createFaces :: [V4 PT]
createFaces = fmap (fmap PT)
              $ [V4 1 3 7 5
                ,V4 6 7 5 4
                ,V4 0 4 5 1
                ,V4 1 5 7 3
                ,V4 3 7 6 2
                ,V4 2 6 4 0
                ]

-- simple and direct grid of triangles.
tessellation :: V2 Int -> Mesh V3 R2
tessellation (V2 x y) = Mesh the_points the_faces
    where
        the_points = V.fromList
            [ A.P $ V2 (fromIntegral u / fromIntegral x)
                       (fromIntegral v / fromIntegral y)
            | u <- us
            , v <- vs
            ]
        the_faces = fmap (fmap ix)
             [ f
             | (u,u') <- us `zip` tail us
             , (v,v') <- vs `zip` tail vs
             , f <- [ V3 (u,v) (u,v') (u',v')
                    , V3 (u',v') (u',v) (u,v)
                    ]
             ]
        ix (u,v) = PT $ u + v * (x + 1)
        us = [0..x] :: [Int]
        vs = [0..y] :: [Int]

-- A Cube Mesh is Quads(V4) in 3D(R3)
cubeMesh :: Mesh V4 R3
cubeMesh = Mesh
  { points = V.fromList
          [ A.P $ V3 x y z
          | x <- [-1,1]
          , y <- [-1,1]
          , z <- [-1,1]
          ]
  , faces  = fmap (fmap PT)
           $ [V4 0 1 3 2
             ,V4 6 7 5 4
             ,V4 0 4 5 1
             ,V4 1 5 7 3
             ,V4 3 7 6 2
             ,V4 2 6 4 0
             ]
  }
