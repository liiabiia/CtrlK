{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}

module Graphics.Tonganoxie.Distance where

import Data.List

--distance to formula DISTANCE TO LINE
distanceTo :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
distanceTo (x1, y1) (x2, y2) (xp, yp) = (abs ((y2-y1)*xp - (x2-x1)*yp + x2*y1 - y2*x1) ) / denom
  where
    denom = if sqrt ((y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)) > 0.0000000001 then sqrt ((y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)) else 0.0000000001


slope :: (Double, Double) -> (Double, Double) -> Double
slope (x1, y1) (x2, y2) = ((y2-y1)/(x2-x1))

yIntercept :: (Double, Double) -> (Double, Double) -> Double
yIntercept p1@(x1, y1) p2@(x2, y2) = yInt
  where
    slope' = slope p1 p2
    yInt = y1 - (slope' * x1)


findX :: (Double, Double) -> (Double, Double) -> Double -> Double
findX (x1, y1) (x2, y2) ypt =
  let m = slope (x1, y1) (x2, y2)
  in (((ypt - y1) / m ) + x1)

findY :: (Double, Double) -> (Double, Double) -> Double -> Double
findY (x1, y1) (x2, y2) xpt =
  let m = slope (x1, y1) (x2, y2)
  in m*(xpt - x1) + y1

--finds a list of (Double, Double) equal spacing over num between (x1, y1) and (x2, y2)
findPointsBetween :: Int -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
findPointsBetween num (x1, y1) (x2, y2) = points'
  where
    points' = points
    points = [(x1 + x' * ((fromIntegral num')/ (fromIntegral (length nums) - 1)), y1 + y' * ((fromIntegral num')/ (fromIntegral (length nums) - 1))) | num' <- nums ]
    x' = (x2 - x1)
    y' = (y2 - y1)
    nums = [0..(num-1)] :: [Int]
