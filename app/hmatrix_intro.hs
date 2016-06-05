{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

--import System.Environment
--import Numeric.Container
{-import Codec.Picture ()-}
import "gtk" Graphics.UI.Gtk (imageNewFromFile,
                        initGUI,
                        windowNew,
                        set,
                        windowDefaultWidth,
                        windowDefaultHeight,
                        onDestroy,
                        containerAdd,
                        mainQuit,
                        vBoxNew,
                        containerAdd,
                        boxPackStart,
                        AttrOp(..),
                        Packing(..),
                        widgetShowAll,
                        mainGUI )
--import Numeric.LinearAlgebra.Algorithms (det)
import MyRepaToHMatrix (repaToMatrix, matrixToRepa)
import Huffman
import qualified Test.QuickCheck as Q
import Data.List (sort, group)
import Data.List.Split (chunksOf)
--import Data.Packed.Repa (matrixToRepa)
import Data.Word (Word8)
import qualified Data.Array.Repa as R (map, computeUnboxedP)
import Data.Array.Repa.Index
import Data.Array.Repa.IO.BMP (readImageFromBMP, writeImageToBMP)
import qualified Data.Either.Combinators as E
import Data.Array.Repa.Repr.Unboxed as RV
--import qualified Data.Array.Repa.Shape as SHAPE
--import Data.Packed.Vector (fromList, toList)
--import qualified Data.Packed.Matrix as M
--import qualified Numeric.LinearAlgebra.LAPACK as LAPACK
import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as VEC
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as BIN (Binary (..), encode, decode)
import Control.Arrow
import Control.Monad.ST (runST)
import Debug.Trace

instance Element Int
instance Container Vector Int

main :: IO ()
main = do
   --args <- getArgs
   let inputFile = "in.bmp"
   image <- imageNewFromFile inputFile
   _ <- initGUI
   win <- windowNew
   set win [windowDefaultWidth := 800, windowDefaultHeight := 600]
   _ <- onDestroy win mainQuit
   vbox <- vBoxNew False 2
   containerAdd win vbox
   print "hmatrix"
   rimg <- readImageFromBMP inputFile
   let repam = E.fromRight' rimg
   let env = setUpEnv CFDCT 256 24.0
   let li = [repaBMPToImage env repam] 
   let ei = map (encodeImage env) li
   let di = map (decodeImage env) ei
   let drepa = map imageToRepaBMP di
   writeImageToBMP "out.bmp" (last drepa)
   mirrage <- imageNewFromFile "out.bmp"
   boxPackStart vbox image PackNatural 0
   boxPackStart vbox mirrage PackNatural 1
   widgetShowAll win
   print $ map L.length ei
   print $ meanAbsDistance (p1 $ head $ tiles $ head li) (p1 $ head $ tiles $ head di)
   print $ meanAbsDistance (p2 $ head $ tiles $ head li) (p2 $ head $ tiles $ head di)
   print $ meanAbsDistance (p3 $ head $ tiles $ head li) (p3 $ head $ tiles $ head di)
   print $ meanSquareDistance (p1 $ head $ tiles $ head li) (p1 $ head $ tiles $ head di)
   print $ meanSquareDistance (p2 $ head $ tiles $ head li) (p2 $ head $ tiles $ head di)
   print $ meanSquareDistance (p3 $ head $ tiles $ head li) (p3 $ head $ tiles $ head di)
   {-print $ head li-}
   {-print $ head di-}
   mainGUI

frequency :: Ord a => [a] -> [(Int, a)]
frequency = map (length &&& head) . group . sort

instance (Q.Arbitrary  a, VEC.Storable a) => Q.Arbitrary (Matrix a) where
   arbitrary = do
      Q.Positive bs <- Q.arbitrary
      l <- Q.vector $ bs * bs
      return ((bs><bs) l)

data Tile a = Tile { p1 :: Matrix a, p2 :: Matrix a, p3 :: Matrix a }
   deriving Show
instance (Q.Arbitrary a, VEC.Storable a) => Q.Arbitrary (Tile a) where
   arbitrary = do
      let bs = 8
      --Q.Positive bs <- Q.arbitrary
      l1 <- Q.vector $ bs * bs
      l2 <- Q.vector $ bs * bs
      l3 <- Q.vector $ bs * bs
      return (Tile ((bs><bs) l1) ((bs><bs) l2) ((bs><bs) l3))
instance (Unbox a, VEC.Storable a, Eq a, Element a) => Eq (Tile a) where
   (==) t1 t2 = (conv p1 t1 == conv p1 t2) && (conv p2 t1 == conv p2 t2) && (conv p3 t1 == conv p3 t2)
      where
         conv p tile = RV.toUnboxed $ matrixToRepa $ p tile

-- ts = tile side length
data Image a = Image { tiles :: [Tile a], width :: Int, height :: Int, ts :: Int }
   deriving Show
instance (Q.Arbitrary a, VEC.Storable a) => Q.Arbitrary (Image a) where
   arbitrary = do
      let bs = 8
      Q.Positive wm <- Q.arbitrary
      Q.Positive hm <- Q.arbitrary
      tls <- Q.vector $ wm * hm
      return (Image tls (wm * bs - 2) (hm * bs - 3) bs)

data EncodedImage = EncodedImage { etiles :: [L.ByteString], ewidth :: Int, eheight :: Int, ets :: Int }

data CFunc = CFDCT | CFHAAR deriving (Eq)

data Plane = Plane1 | Plane2 | Plane3

data ImageEnv = ImageEnv {
   tMat           :: Matrix Double,
   coeffFunc      :: CFunc,
   tileSize       :: Int,
   perm           :: UV.Vector Int,
   invPerm        :: UV.Vector Int,
   quantizeDouble :: Double
   }
instance Eq ImageEnv where
   (==) (ImageEnv _ cf1 ts1 _ _ qi1) (ImageEnv _ cf2 ts2 _ _ qi2) = (cf1 == cf2) && (ts1 == ts2) && (qi1 == qi2)

instance BIN.Binary EncodedImage where
   put (EncodedImage et ew eh ebs) = do   BIN.put et
                                          BIN.put ew
                                          BIN.put eh
                                          BIN.put ebs
   get                             = do   et  <- BIN.get 
                                          ew  <- BIN.get
                                          eh  <- BIN.get
                                          ebs <- BIN.get
                                          return (EncodedImage et ew eh ebs)

setUpEnv :: CFunc -> Int -> Double -> ImageEnv
setUpEnv f bs q = ImageEnv (transMat bs (getCoeffFunc f)) f bs p ip q
   where
      p = UV.fromList $ reorderPerm 0 0 bs
      invmap = Prelude.zip (reorderPerm 0 0 bs) [0 .. ]
      ip = UV.generate (bs * bs) id UV.// invmap

-- TODO : implement, prepend to globalEnv, change de/quantize to accept the quantization var as parameter
-- converts a repa array of rgb tuples to a tiled image
repaBMPToImage :: ImageEnv -> Array U DIM2 (Word8, Word8, Word8) -> Image Double
repaBMPToImage ie ra = Image tls w h (tileSize ie)
   where
      (my, mCb, mCr) = repaTo3Planes $ runST $ R.computeUnboxedP $ R.map rgbToYCbCr ra
      tls = (map toTile . convertTuple . map3 (tileMatrix (tileSize ie))) (my, mCb, mCr)
      h = rows my
      w = cols my

imageToRepaBMP :: Image Double -> Array U DIM2 (Word8, Word8, Word8)
imageToRepaBMP i = runST $ R.computeUnboxedP $ R.map yCbCrToRGB (RV.zip3 ry rcb rcr)
   where
      bigTile = fromImage i
      [ry, rcb, rcr] = map matrixToRepa [p1 bigTile, p2 bigTile, p3 bigTile]

encodeImage :: ImageEnv -> Image Double -> L.ByteString
encodeImage ie im@(Image _ w h bs) =  BIN.encode (EncodedImage het w h bs)
   where
      -- tranformed and quantized image
      (Image qtt _ _ _) = mapImage (applyToPlanes (quantize (quantizeDouble ie)) . transform2D haarCoeff) im
      -- first run length encode then transform to huffman encoded tiles [L.Bytestring]
      het = [ (huffmanEncode . rle) l | l <- lists ]
      -- [UV.Vector Int]
      lists = toVList ie qtt

decodeImage :: ImageEnv -> L.ByteString -> Image Double
decodeImage ie bytes = mapImage (itransform2D ie . applyToPlanes (dequantize $ quantizeDouble ie)) (Image tls ew eh ebs)
   where
      (EncodedImage het ew eh ebs) = BIN.decode bytes :: EncodedImage
      -- [L.ByteString] --> [UV.Vector Int] --> run length decode --> [Tile]
      tls = fromVList ie [ rld (huffmanDecode t :: (UV.Vector Int)) | t <- het ]

qcEnDecodeImage :: Image Double -> Bool
qcEnDecodeImage i = meanAbsDistance (p1 $ head $ tiles i) (p1 $ head $ tiles deci) <= 3
   where
      deci = (decodeImage env . encodeImage env) i
      env = setUpEnv CFDCT (ts i) 16.0

repaTo3Planes :: Array U DIM2 (Double, Double, Double) -> (Matrix Double, Matrix Double, Matrix Double)
repaTo3Planes t =
  (repaToMatrix (getP Plane1 unzipped),
   repaToMatrix (getP Plane2 unzipped),
   repaToMatrix (getP Plane3 unzipped))
   where unzipped = RV.unzip3 t

-- fill matrix to multiple of block size
--fillMatrix :: forall a. (Element a, Floating a, Container Vector a, Transposable (Matrix a) (Matrix a)) => Int -> Matrix a -> Matrix a
fillMatrix :: Int -> Matrix Double -> Matrix Double
fillMatrix bs m
   | (margin_right == bs) && (margin_bottom == bs) = m
   | margin_right == bs = fromBlocks [[m], [b]]
   | margin_bottom == bs = fromBlocks [[m, r]]
   | otherwise = fromBlocks [[ m , r ], [ b , br ]]
   where
      r = fromLists [ replicate margin_right (m `atIndex` (i, w - 1)) | i <- [0 .. (h - 1)] ]
      b = tr $ fromLists [ replicate margin_bottom (m `atIndex` (h - 1, i)) | i <- [0 .. (w -1)] ]
      br = (margin_bottom >< margin_right) $ repeat (m `atIndex` (h - 1, w - 1))
      margin_right = bs - (w `mod` bs)
      margin_bottom = bs - (h `mod` bs)
      h = rows m
      w = cols m

cutMatrix :: Element a => Int -> Int -> Matrix a -> Matrix a
cutMatrix w h m = (head . head) (toBlocks [w] [h] m)

-- bs = block size
--tileMatrix :: (Element a, Show a, Num a, Container Vector a, Transposable (Matrix a) (Matrix a)) => Int -> Matrix a -> [Matrix a]
tileMatrix :: Int -> Matrix Double -> [Matrix Double]
tileMatrix bs m = concat $ toBlocksEvery bs bs (fillMatrix bs m)

fromImage :: (Element a, Show a) => Image a -> Tile a
fromImage (Image tls w h bs) = trace ("BlocksInRow: " ++ show blocksInRow ++ " Blocksize: " ++ show bs ++ " w: " ++ show w ++ " h: " ++ show h ++ " num tiles: " ++ show (length mys)) $ mapTile (cutMatrix h w) (Tile (fromMatrices mys) (fromMatrices mcbs) (fromMatrices mcrs))
   where
      fromMatrices ms = fromBlocks $ chunksOf blocksInRow ms
      blocksInRow = ceiling ((fromIntegral w / fromIntegral bs) :: Double)
      mys = map p1 tls
      mcbs = map p2 tls
      mcrs = map p3 tls

toTile :: (Matrix a, Matrix a, Matrix a) -> Tile a
toTile (d, e, f) = Tile d e f

fromVList :: ImageEnv -> [UV.Vector Int] -> [Tile Int]
fromVList ie lli
   | (length lli `mod` 3) /= 0 = error "fromLists list length not divisible by 3"
   | otherwise = [ Tile (vectorToMatrix v1) (vectorToMatrix v2) (vectorToMatrix v3) | [v1, v2, v3] <- chunksOf 3 frlli ]
   where
      -- first we need to inverse frequency reorder
      frlli = map (invFrequencyReorder ie) lli
      vectorToMatrix v = repaToMatrix $ RV.fromUnboxed shape v
      bs = tileSize ie
      shape = Z :. (bs :: Int) :. (bs :: Int)

toVList :: ImageEnv -> [Tile Int] -> [UV.Vector Int]
toVList ie tl = concat [ map (frequencyReorder ie . RV.toUnboxed . matrixToRepa) [m1, m2, m3] | (Tile m1 m2 m3) <- tl ]

-- TODO : implement QuickCheck test for to/fromLists
qcFromToLists :: ImageEnv -> [Tile Int] -> Bool
qcFromToLists _ [] = True
qcFromToLists ie tls = fromVList ie (toVList env tls) == tls
   where
      bs = cols $ p1 $ head tls
      env = setUpEnv CFHAAR bs 16.0

-- applys fun to the Tile's respective planes
applyToPlanes :: (Matrix a -> Matrix b) -> Tile a -> Tile b
applyToPlanes fun t = Tile { p1 = fun (p1 t), p2 = fun (p2 t), p3 = fun (p3 t) }

-- get a specific plane out of a Repa Array Tuple
getP :: Plane -> (Array U DIM2 a, Array U DIM2 a, Array U DIM2 a) -> Array U DIM2 a
getP Plane1 ( p , _ , _ ) = p
getP Plane2 ( _ , p , _ ) = p
getP Plane3 ( _ , _ , p ) = p

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (g, h, i) = (f g, f h, f i)

convertTuple :: ([a], [b], [c]) -> [(a, b, c)]
convertTuple (d, e, f) = Prelude.zip3 d e f

mapTile :: (Matrix a -> Matrix b) -> Tile a -> Tile b
mapTile f (Tile g h i) = Tile (f g) (f h) (f i)

mapImage :: (Tile a -> Tile b) -> Image a -> Image b
mapImage f (Image tls w h tilesize) = Image (map f tls) w h tilesize

-- transformation matrix for color conversion
ccTransmat :: Matrix Double
ccTransmat = (3><3) ([0.299, 0.587, 0.114, -0.168736, -0.331264, 0.5, 0.5, -0.418688, -0.081312] :: [Double])

rgbToYCbCr :: (Word8, Word8, Word8) -> (Double, Double, Double)
rgbToYCbCr (wr, wg, wb) = (y, cb + 128, cr + 128)
   where
      vec = (fromList $ map fromIntegral [wr, wg, wb]) :: Vector Double
      [y, cb, cr] = toList $ ccTransmat #> vec :: [Double]

yCbCrToRGB :: (Double, Double, Double) -> (Word8, Word8, Word8)
yCbCrToRGB (y, cb, cr) = map3 (wordBound . round) (y + 1.402 * (cr - 128), y - 0.34414 * (cb - 128) - 0.71414 * (cr - 128), y + 1.772 * (cb - 128))

wordBound :: Int -> Word8
wordBound i
   | i < 0 = 0
   | i > 255 = 255
   | otherwise = fromIntegral i

-- inverse transformation matrix is the transpose of the transformation matrix
transMat :: Int -> (Int -> Double -> Double -> Double) -> Matrix Double
transMat n cf = build (n, n) (cf n)

getCoeffFunc :: CFunc -> Int -> Double -> Double -> Double
getCoeffFunc CFDCT = dctCoeff
getCoeffFunc CFHAAR = haarCoeff

dctCoeff :: Int -> Double -> Double -> Double
dctCoeff n i j = c0 i * sqrt (2.0 / fromIntegral n) * cos (((2 * j + 1) * i) * pi / fromIntegral (2 * n))

haarCoeff :: Int -> Double -> Double -> Double
haarCoeff n i j = hw i (j / fromIntegral n) (fromIntegral n)

roundToPow2 :: Int -> Int
roundToPow2 k = floor $ logBase 2 (fromIntegral k :: Double)

-- the haar wavelet function
hw :: Double -> Double -> Double -> Double
hw 0.0 _ n = 1.0 / sqrt n
hw k t n
   | t < lower = 0.0
   | t >= higher = 0.0
   | t < mid = 2.0 ** (pk / 2.0) / sqrt n
   | t >= mid = (-1.0) * 2.0 ** (pk / 2.0) / sqrt n
   | otherwise = 0.0 -- this should actually never happen
   where
      pk = fromIntegral $ roundToPow2 $ round k
      lower = (k / (2 ** pk)) - 1.0
      mid = ((k + 0.5) / (2 ** pk)) - 1.0
      higher = ((k + 1.0) / (2 ** pk)) - 1.0

c0 :: Double -> Double
c0 0 = 1.0 / sqrt 2.0
c0 _ = 1.0

transform2D :: (Int -> Double -> Double -> Double) -> Tile Double -> Tile Double
transform2D coeffFunc dt = applyToPlanes transformation dt
   where
      tm = transMat (rows (p1 dt)) coeffFunc
      transformation plane = (tm <> plane) <> tr' tm


itransform2D :: ImageEnv -> Tile Double -> Tile Double
itransform2D ie dt = applyToPlanes transformation dt
   where
      transformation plane = tr' tm <> (plane <> tm)
      tm = tMat ie

quantize :: Double -> Matrix Double -> Matrix Int
quantize i = cmap (round . (/ i))

dequantize :: Double -> Matrix Int -> Matrix Double
dequantize i = cmap ((* i) . fromIntegral)

-- zig zag reordering of the matrix coefficients for better compression
reorderPerm :: Int -> Int -> Int -> [Int]
reorderPerm i j len
   | (i == (len - 1)) && (j == (len - 1)) = [e]
   | (i == 0) && (j < (len - 1)) = e : reorderPerm (j + 1) 0 len
   | j == (len - 1) = e : reorderPerm (len - 1) (i + 1) len
   | otherwise  = e : reorderPerm (i - 1) (j + 1) len
   where e = len * j + i

frequencyReorder :: ImageEnv -> UV.Vector Int -> UV.Vector Int
frequencyReorder ie v = UV.backpermute v (perm ie)

invFrequencyReorder :: ImageEnv -> UV.Vector Int -> UV.Vector Int
invFrequencyReorder ie v = UV.backpermute v (invPerm ie)

qcFreqReorder :: Matrix Int -> Bool
qcFreqReorder m = invFrequencyReorder env (frequencyReorder env v) == v
   where
      v = RV.toUnboxed $ matrixToRepa m
      bs = rows m
      env = setUpEnv CFDCT bs 1

meanSquareDistance :: Matrix Double -> Matrix Double -> Double
meanSquareDistance m1 m2 = sumElements (diffm * diffm) / fromIntegral (msize * msize)
   where
      diffm = m1 - m2
      msize = rows m1

meanAbsDistance :: Matrix Double -> Matrix Double -> Double
meanAbsDistance m1 m2 = sumElements (cmap abs (m1 - m2)) / fromIntegral (msize * msize)
   where
      msize = rows m1

-- (mean square distance check to original)
-- write image
