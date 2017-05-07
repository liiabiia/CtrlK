--module Main where
--alt z


--imports for graphics
import Graphics.Tonganoxie.Object as O
import Graphics.Tonganoxie.Material
import Graphics.Tonganoxie.Shapes
import Graphics.Tonganoxie.Tessellation as T
import Graphics.Tonganoxie.Types
import Graphics.Tonganoxie.Surface as S
import Linear.Quaternion.Utils

--imports for GUI
import Graphics.Blank as BC
import qualified Data.Text as T

--other useful utilities
import Data.Vector (Vector, toList)
import qualified Data.Vector as V
import Linear.V2 (V2(V2))
import Linear.V3 (V3(V3), cross)
import Linear.V4 (V4(V4))
import qualified Linear.Affine as A
import Linear.Metric(normalize, distance)
import Codec.Picture as P

--Liia imports
import Graphics.Tonganoxie.PolyTransform
--test:: FilePath -> IO Object
--test x = readObject x

--printThis :: String -> ImageData
--printThis img = getImageData( newImage (T.pack img))


recUVMaterial = uvMaterial (T.pack "Material") [ Kd 1 1 1 , Map_Kd (T.pack "testImage.jpg") , Illum 1]

try = addUVMaterial recUVMaterial
--need clicks
--move to first click
--lineto second click

--create dots and lines
--connect first dots together, connect second dots together

--hit a key then it transforms into obj

--get context

fp :: String -> FilePath
fp s = s

getimg :: (Either String DynamicImage) -> P.DynamicImage
getimg img = case img of
  Right x -> x

transformThis :: String -> IO ()
transformThis img = blankCanvas 3000 {events = [(T.pack "mousedown")]} $ \ context -> do
    let w = width context
    let h = height context
    print w
    print h
    --let scaleFactor = fixSize image (w, h) (1.0, 1.0)
    send context $ do
      image <- newImage (T.pack img)
      let imgW = width image
      let imgH = height image
      let (x, y) = fixSize (imgW, imgH) (w,h) (1.0, 1.0)
      drawImage(image, [0,0, imgW * x, imgH * y])
    --print context
    transformThisLoop img context

transformThisLoop :: String -> DeviceContext -> IO ()
transformThisLoop img context = do
  image <- readImage (fp img)
  --let imgw' = width (newImage (T.pack img))
  let image' = convertRGB8 (getimg image)
  let w' = width context
  let h' = height context
  let iw' = imageWidth image'
  let ih' = imageHeight image'
  print w'
  print h'
  print iw'
  print ih'
  let (x', y') = fixSize (fromIntegral iw', fromIntegral ih') (w', h') (1.0, 1.0)
  let iw = (fromIntegral iw') * x'
  let ih = (fromIntegral ih') * y'
  print iw
  print ih
  pt1 <- wait context
  pt2 <- wait context
  --draw line between pt1 and pt2
  drawDiagonal context pt1 pt2
  pt3 <- wait context
  --draw line between pt1 and pt3
  drawDiagonal context pt1 pt3
  pt4 <- wait context
  --draw line between pt3 and pt4
  drawDiagonal context pt3 pt4
  pt5 <- wait context
  --draw line between pt1 and pt5
  drawDiagonal context pt1 pt5
  pt6 <- wait context
  --draw line between pt5 and pt6
  drawDiagonal context pt5 pt6
  pt7 <- wait context
  --draw line between pt5 and pt7, pt3 and pt7
  drawDiagonal context pt5 pt7
  drawDiagonal context pt3 pt7
  pt8 <- wait context
  --draw line between p7 and pt8
  drawDiagonal context pt7 pt8




  --begin image transformation
  let (x1, y1) = getPoint context pt1
  let (x2, y2) = getPoint context pt2
  let (x3, y3) = getPoint context pt3
  let (x4, y4) = getPoint context pt4
  let (x5, y5) = getPoint context pt5
  let (x6, y6) = getPoint context pt6
  let (x7, y7) = getPoint context pt7
  let (x8, y8) = getPoint context pt8

  --check boundaries for each--

  --inner rectangle
  --top trap
  --right trap
  --bottom trap
  --left trap
  --outer rectangle

  --width and height
  --width is avg of the top and bottom of inner rectangle
  let w = ((calcLine x1 x3) + (calcLine x5 x7)) / 2.0

  --height is avg of top and bottom of inner rectangle
  let h = ((calcLine y1 y5) + (calcLine y3 y7)) / 2.0

  --calculate area of all 6 surfaces
  let aInner = (calc2DA (calcLine x1 x3) (calcLine x5 x7) (calcLine y1 y5))
  let aTop = (calc2DA (calcLine x1 x3) (calcLine x2 x4) (calcLine y1 y2))
  let aLeft = (calc2DA (calcLine x1 x5) (calcLine x2 x6) (calcLine y1 y2))
  let aBottom = (calc2DA (calcLine x5 x7) (calcLine x6 x8) (calcLine y5 y6))
  let aRight = (calc2DA (calcLine x3 x7) (calcLine x4 x8) (calcLine y3 y4))
  let aOut = (calc2DA (calcLine x2 x4) (calcLine x6 x8) (calcLine x2 x6))

  let sa = calcSurfArea aInner aTop aRight aBottom aLeft aOut

  let l = calcLength sa w h

  --have inner rectangle start at (0 - w/2, 0, 0)
  let (ix1, iy1, iz1) = ( 0.0 - w / 2.0, 0.0, 0.0)
  let (ix2, iy2, iz2) = (0.0 + w/2.0, 0.0, 0.0)
  let (ix3, iy3, iz3) = (0.0 - w/2.0, h, 0.0)
  let (ix4, iy4, iz4) = (0.0 + w/2.0, h, 0.0)
  --back rectangle
  let (ix5, iy5, iz5) = (0.0 - w/2.0, 0.0, 0.0 + l)
  let (ix6, iy6, iz6) = (0.0 + w/2.0, 0.0, 0.0 + l)
  let (ix7, iy7, iz7) = (0.0 - w/2.0, h, 0.0 + l)
  let (ix8, iy8, iz8) = (0.0 + w/2.0, h, 0.0 + l)

  let v1 = convertV3 ix1 iy1 iz1
  let v2 = convertV3 ix2 iy2 iz2
  let v3 = convertV3 ix3 iy3 iz3
  let v4 = convertV3 ix4 iy4 iz4
  let v5 = convertV3 ix5 iy5 iz5
  let v6 = convertV3 ix6 iy6 iz6
  let v7 = convertV3 ix7 iy7 iz7
  let v8 = convertV3 ix8 iy8 iz8

  let vs = V.fromList ( map (\ x -> v3ToAPoint x) [v1, v2, v3, v4, v5, v6, v7, v8] )


  let recUVMaterial = uvMaterial  (T.pack "Material") [ Ka 1 1 1 , Kd 1 1 1,  Map_Ka (T.pack img) , Map_Kd (T.pack img),  Illum 5]
  --let tryThis = mkMT recUVMaterial
  let inRecUVMaterial = V.fromList [recUVMaterial]

  let n1 = calcVertexNormal v1 v2 v3
  let n2 = calcVertexNormal v1 v2 v6
  let n3 = calcVertexNormal v7 v6 v2
  let n4 = calcVertexNormal v8 v7 v4
  let n5 = calcVertexNormal v8 v5 v1
  let n6 = calcVertexNormal v5 v6 v7

  let ns = V.fromList [n1, n2, n3, n4, n5, n6]

    -- this is V4 R3
  --need g R2 mesh
  --create mesh out of the picture
  --concatenate all the bits of pictures together? ***DO THIS IN MESH
  --mappend
  --let m =
  --let (V2 u1, v1) = (V2 x1 y1)
  let uvPts = [(xyToUV (V2 x1 y1) (iw, ih)), (xyToUV (V2 x2 y2) (iw, ih)), (xyToUV (V2 x3 y3) (iw, ih)), (xyToUV (V2 x4 y4) (iw, ih)), (xyToUV (V2 x5 y5) (iw, ih)), (xyToUV (V2 x6 y6) (iw,ih)), (xyToUV (V2 x7 y7) (iw, ih)), (xyToUV (V2 x8 y8) (iw, ih))]
  --print (xyToUV (V2 x1 y1) (iw, ih))
  --print (xyToUV (V2 x2 y2) (iw, ih))
  --print (xyToUV (V2 x3 y3) (iw, ih))
  --print (xyToUV (V2 x4 y4) (iw, ih))
  --print (xyToUV (V2 x5 y5) (iw, ih))
  --print (xyToUV (V2 x6 y6) (iw, ih))
  --print (xyToUV (V2 x7 y7) (iw, ih))
  --print (xyToUV (V2 x8 y8) (iw, ih))


  --print(xyToUV (V2 x4 y4) (iw, ih))

  --print(xyToUV (V2 x8 y8) (iw, ih))

  --let uvPts2 = [(V2 x1 y1), (V2 x2 y2), (V2 x3 y3), (V2 x4 y4), (V2 x5 y5), (V2 x6 y6), (V2 x7 y7), (V2 x8 y8)]
  --let please = uvShape (S.sphere) (T.tessellation (V2 4 4)) recUVMaterial
  --let please = uvShape' uvPts l w h (T.roomMesh uvPts) recUVMaterial
  --let please = uvShape' uvPts (l / (l+w+h)) (w/ (l+w+h)) (h/ (l+w+h)) (T.tessellation (V2 100 100)) recUVMaterial
  let a = (round (iw / 100))
  let b = (round (ih / 100))

  --let t = T.createTess uvPts
  --let t = T.tessellation (V2 5 5)

  let t = T.createTess uvPts 4

  --print (length (V.toList (T.points t)))

  --print (take 9 (V.toList (T.points t)))
  --print (T.faces t)
  print l
  print w
  print h

  let dims = calcDimensions w h l

  print dims

  let please = uvShape' uvPts dims t recUVMaterial --(l / (l+w+h)) (w/ (l+w+h)) (h/ (l+w+h)) t recUVMaterial

  --let please = uvShape S.plane (T.tessellation' (V2 a b) 1) recUVMaterial

  --let please = uvShape S.plane (T.tessellation' (V2 (round iw) (round ih)) 100) recUVMaterial
  --let please = uvShape S.plane (T.tessellation' (V2 (round iw) (round ih))) recUVMaterial --(T.tessellation' (V2 (round x2) (round y2)) (V2 (round x4) (round y4)) (V2 (round x6) (round y6)) (V2 (round x8) (round y8)) (round iw, round ih)) recUVMaterial

  --let inRecObj = Object { O.points = vs, normals = V.empty, uvs = V.empty, materials = V.empty, uv_materials = inRecUVMaterial, O.faces = [] } --, Vector (ptObj2), Vector (ptObj3), Vector (ptObj4) }

  --writeObject "test1.obj" inRecObj
  writeObject "test1.obj" please


  --map a plane, plane is a mesh
  --get points, get normals,
  --3d pt conversion
  --do a let for each plane, map coordinates of the image with the plane for the material, make a create plane
  --do a let for the object, put everything together
  --concatinate the files together for one file
  --see if you can do a "writeObject for a list of objects
  --
  --transformThisLoop img context

--we have to write an object
--writeObject at the end
--draw diagonal
drawDiagonal :: DeviceContext -> Event -> Event -> IO ()
drawDiagonal context ptA ptB =
  case ePageXY ptA of
    Nothing -> return ()
    Just (x1, y1) -> case ePageXY ptB of
      Nothing -> return ()
      Just(x2, y2) -> send context $ do
        moveTo (x1, y1)
        lineTo(x2, y2)
        lineWidth 5
        strokeStyle (T.pack "blue")
        stroke()

--get points
getPoint :: DeviceContext -> Event -> (Double, Double)
getPoint context pt =
  case ePageXY pt of
    Just (x, y) -> (x, y)
    --Nothing?


--calculate line
calcLine s f = abs(f - s)

--calculate 2D area
--l1 is first line, l2 is second line, h1 is the height of l1, h2 is the height of l2
calc2DA l1 l2 h = ((l1 + l2) / 2.0 ) * abs h


--calculate surface area
calcSurfArea sa1 sa2 sa3 sa4 sa5 sa6 = sa1 + sa2 + sa3 + sa4 + sa5 + sa6

--calculate length using formula
calcLength sa w h = (sa - 2.0 * (w * h)) /  (2.0 * (h + w))


--convert doubles to v3 pts
convertV3 :: Double -> Double -> Double -> V3 Double
convertV3 x y z = V3 x y z

v3ToAPoint :: V3 Double -> A.Point V3 Double
v3ToAPoint x = A.P x


calcVertexNormal :: V3 Double -> V3 Double -> V3 Double -> V3 Double
calcVertexNormal p1 p2 p3 = normalize $ (p2 - p1) `cross` (p3 - p1)

--XY to UV conversion
xyToUV :: V2 Double -> (Double, Double) -> V2 Double
xyToUV (V2 x y) (w, h) = (V2 (x / w)  ((h-y) / h))



--click for Fold
--click for Cut

calcDimensions :: Double -> Double -> Double -> [Double]
calcDimensions w h l = [w', h', l']
  where
    w' = (w / x)
    h' = (h / x)
    l' = (l / x)
    x = minimum[w, h, l]



--rescale
rescale (x, y) = (1.0 / x, 1.0 / y )

--fix size
fixSize :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
fixSize (imgW, imgH) (w, h) (x, y) = if ( (imgW * x) <= w && (imgH * y) <= h ) then (x, y) else
  (fixSize (imgW, imgH) (w, h) (x-0.05, y-0.05))
