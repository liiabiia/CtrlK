{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}

module Graphics.Tonganoxie.Distance where

import Data.List

shouldInclude :: [(Double, Double)] -> Bool
shouldInclude [] = True
shouldInclude (x : xs) = if (not $ shouldInclude' x xs) then False else shouldInclude xs

shouldInclude' :: (Double, Double) -> [(Double, Double)] -> Bool
shouldInclude' (x,y) [] = True
shouldInclude' (x,y) ((x', y') : xs ) = if ((distanceLine (x,y) (x',y')) <= 0.00001^^2) then False else shouldInclude' (x, y) xs


round5' :: Double -> Int
round5' x = truncate (x * 10^5)

round5 :: Double -> Double
round5 x = (fromInteger $ truncate $ x * (10^5)) / (10.0^^5)

distanceLine :: (Double, Double) -> (Double, Double) -> Double
distanceLine (x1, y1) (x2, y2) = sqrt ((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))

distancePoints :: Double -> Double -> Double
distancePoints x1 x2 = abs (x2 - x1)

disPtAvg :: Double -> Double -> Double -> Double
disPtAvg xp xRef xOS = (distancePoints xp xRef) / (distancePoints xRef xOS)



--distance to formula DISTANCE TO LINE
distanceTo :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
distanceTo (x1, y1) (x2, y2) (xp, yp) = (abs ((y2-y1)*xp - (x2-x1)*yp + x2*y1 - y2*x1) ) / denom
  where
    denom = if sqrt ((y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)) > 0.0000000000001 then sqrt ((y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)) else 0.0000000000001 


slope :: (Double, Double) -> (Double, Double) -> Double
slope (x1, y1) (x2, y2) = ((y2-y1)/(x2-x1))

yIntercept :: (Double, Double) -> (Double, Double) -> Double
yIntercept p1@(x1, y1) p2@(x2, y2) = yInt
  where
    slope' = slope p1 p2
    yInt = y1 - (slope' * x1)

makeLine :: (Double, Double) -> (Double, Double) -> (Double, Double)
makeLine p1@(x1, y1) p2@(x2, y2) = (m, b)
  where
    m = slope p1 p2
    b = yIntercept p1 p2



findX :: (Double, Double) -> (Double, Double) -> Double -> Double
findX (x1, y1) (x2, y2) ypt = if (abs (x2 - x1) > 0) then xpt else x1
  where
      m = slope (x1, y1) (x2, y2)
      xpt = (((ypt - y1) / m ) + x1)


--Do not use findY on a line that could be vertical
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


{-findPointsBetweenX :: Int -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
findPointsBetweenX num (x1, y1) (x2, y2) = points'
  where
    points' = findYs 0 num (x1, y1) (x2, y2) points
    points = [(x1 + (sqrt(x * y)*((fromIntegral cur) / (fromIntegral (length nums) -1)))) / (sqrt (x*y)) | cur <- nums]
    x = (distancePoints x1 x2)
    y = (distancePoints y1 y2)
    nums = [0..(num-1)] :: [Int]

findYs :: Int-> Int -> (Double, Double) -> (Double, Double) -> [Double] -> [(Double, Double)]
findYs cur num (x1, y1) (x2, y2) [] = []
findYs cur num (x1, y1) (x2, y2) (x:xs) = if (abs (round5' (x1-x2)) > 0) then [(x, (findY (x1, y1) (x2, y2) x))] ++ findYs (cur+1) num (x1, y1) (x2,y2) xs
    else
      [(x, (y1 + y' * ((fromIntegral cur)/ (fromIntegral (length nums) - 1))))] ++ findYs (cur+1) num (x1, y1) (x2, y2) xs
      where
        y' = (y2 - y1)
        nums = [0.. (num-1)] :: [Int]

findPointsBetweenY :: Int -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
findPointsBetweenY num (x1, y1) (x2, y2) = points'
  where
    points' = findXs 0 num (x1, y1) (x2, y2) points
    points = [(y1 + (sqrt(x * y)*((fromIntegral cur) / (fromIntegral (length nums) -1)))) / (sqrt (x*y)) | cur <- nums]
    x = (distancePoints x1 x2)
    y = (distancePoints y1 y2)
    nums = [0..(num-1)] :: [Int]

findXs :: Int-> Int -> (Double, Double) -> (Double, Double) -> [Double] -> [(Double, Double)]
findXs cur num (x1, y1) (x2, y2) [] = []
findXs cur num (x1, y1) (x2, y2) (y:ys) = if (abs (round5' (x1-x2)) > 0) then [(findX (x1, y1) (x2, y2) y, y)] ++ findXs (cur+1) num (x1, y1) (x2,y2) ys
    else
      [(x1 + x' * ((fromIntegral cur)/ (fromIntegral (length nums) - 1)), y)] ++ findYs (cur+1) num (x1, y1) (x2, y2) ys
      where
        x' = (x2 - x1)
        nums = [0.. (num-1)] :: [Int]-}




{-fixX :: (Double, Double) -> (Double, Double)-> (Double, Double)-> (Double, Double)
fixX (xs, ys) (xf, yf) (x, y) = if ((x,y) == (xs,ys)) then (xs, ys) else if ((x,y) == (xf, yf)) then (xf, yf) else (x', y')
  where
    wholeLine = (distanceLine (xs, ys) (xf, yf))
    curLine = (distanceLine (xs, ys) (x, y))
    lineProp = wholeLine / curLine
    xPart = (distancePoints xs xf)
    yPart = (distancePoints ys yf)
    newSquareSide = sqrt( xPart * yPart)
    ptLoc = (lineProp * (distanceLine (0,0) (newSquareSide, newSquareSide)))
    --newProp = sqrt (xPart * yPart)
    x' = ptLoc--(ptLoc) * wholeLine
    y' = if (round5 (xs - x) /= 0) then findY (xs, ys) (xf, yf) x' else y


fixY :: (Double, Double) -> (Double, Double)-> (Double, Double) -> (Double, Double)
fixY (xs, ys) (xf, yf) (x, y) = if ((x,y) == (xs,ys)) then (xs, ys) else if ((x,y) == (xf, yf)) then (xf, yf) else (x', y')
  where
    wholeLine = (distanceLine (xs, ys) (xf, yf))
    curLine = (distanceLine (xs, ys) (x, y))
    lineProp = wholeLine / curLine
    xPart = (distancePoints xs xf)
    yPart = (distancePoints ys yf)
    newSquareSide = sqrt( xPart * yPart)
    ptLoc = (lineProp * (distanceLine (0,0) (newSquareSide, newSquareSide))) / (2/sqrt(2)) + ys
    --newProp = sqrt (xPart * yPart)
    y' = ptLoc--(ptLoc) * wholeLine
    x' = if (round5 (xs - x) /= 0) then findX (xs, ys) (xf, yf) y' else x-}
