{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment ( getArgs )
import qualified Data.Vector.Storable as S
import Data.Word ( Word8 )
import qualified Codec.Picture as Juicy
import Foreign.Storable

data Pixel = RGBA !Word8 !Word8 !Word8 !Word8
  deriving (Show)

instance Storable Pixel where
  sizeOf _ = 4
  alignment _ = alignment (undefined :: Word8)
  peek ptr =
    peekByteOff ptr 0 >>= \r ->
    peekByteOff ptr 1 >>= \g ->
    peekByteOff ptr 2 >>= \b ->
    peekByteOff ptr 3 >>= \a ->
    pure (RGBA r g b a)
  poke ptr (RGBA r g b a) =
    pokeByteOff ptr 0 r >>
    pokeByteOff ptr 1 g >>
    pokeByteOff ptr 2 b >>
    pokeByteOff ptr 3 a 

type Pixels = S.Vector Pixel

data Image = Image {
  pixels :: Pixels,
  width  :: Int,
  height :: Int }

-- Utility

min3 :: Ord a => a -> a -> a -> a
min3 a b = min (min a b)

max3 :: Ord a => a -> a -> a -> a
max3 a b = max (max a b)

-- Clamping

clamp :: Int -> Image -> Image
clamp newWidth image = Image {
    pixels = S.generate newSize clampStep,
    width  = newWidth,
    height = newHeight }
  where
    newSize = newWidth * newHeight
    newHeight = round (fromIntegral (height image) / xStepSize / 2)
    xStepSize = fromIntegral (width image) / fromIntegral newWidth :: Float
    yStepSize = fromIntegral (height image) / fromIntegral newHeight :: Float

    clampStep :: Int -> Pixel
    clampStep i = pixels image `S.unsafeIndex` index
      where
        index = yIndex * width image + xIndex
        yIndex = if (row+1) == newHeight
          then height image - 1
          else floor (fromIntegral row * yStepSize)
        xIndex = if (column+1) == newWidth
          then width image - 1
          else floor (fromIntegral column * xStepSize)
        row = i `div` newWidth :: Int
        column = i `mod` newWidth :: Int

-- Conversion

convert :: Juicy.Image Juicy.PixelRGBA8 -> Image
convert image = Image {
  pixels = S.generate size convertStep,
  width  = imageWidth,
  height = imageHeight }
  where
    size = imageWidth * imageHeight
    imageWidth = Juicy.imageWidth image
    imageHeight = Juicy.imageHeight image

    getImageByte :: Int -> Word8
    getImageByte i = fromIntegral $ imageData `S.unsafeIndex` i
      where imageData = Juicy.imageData image

    convertStep :: Int -> Pixel
    convertStep i = RGBA r g b a
      where
        r = getImageByte start
        g = getImageByte $ start+1
        b = getImageByte $ start+2
        a = getImageByte $ start+3
        start = i * 4

-- ASCII

asciiSymbols :: [Char]
asciiSymbols = [' ', '.', ':', '-', '=', '+', '/', '%', '#', '@']

toAscii :: Pixel -> Char
toAscii (RGBA r g b a) = asciiSymbols !! index
  where
    mn = fromIntegral $ min3 r g b :: Float
    mx = fromIntegral $ max3 r g b :: Float
    lightness = (mn + mx) / 2 / 255 * fromIntegral a / 255 :: Float
    index = round (lightness * fromIntegral maxIndex)
    maxIndex = length asciiSymbols - 1

-- Printing

generateString_rec :: Int -> Int -> (Int -> String) -> String
generateString_rec i n f
  | i == n    = []
  | otherwise = f i ++ generateString_rec (i+1) n f

generateString :: Int -> (Int -> String) -> String
generateString = generateString_rec 0

printAscii :: Juicy.Image Juicy.PixelRGBA8 -> Int -> IO ()
printAscii originalImage maxWidth = putStrLn $ generateString size printAsciiStep
  where
    size = S.length imageAscii
    imageAscii = S.map toAscii (pixels image)
    image = clamp newWidth (convert originalImage)
    newWidth = min maxWidth (Juicy.imageWidth originalImage)

    printAsciiStep :: Int -> String
    printAsciiStep i =
      imageAscii `S.unsafeIndex` i :
      if i > 0 && (i+1) `mod` newWidth == 0
        then "\n" else []

-- User input

getWidth :: IO Int
getWidth =
  let defaultWidth = 80 in
  putStrLn ("Enter output width or nothing for default (" ++ show defaultWidth  ++ "):") >>
  getLine >>= \input ->
  pure $ if null input then defaultWidth else read input

getImage :: String -> IO (Either String (Juicy.Image Juicy.PixelRGBA8))
getImage path = fmap Juicy.convertRGBA8 <$> Juicy.readImage path

main :: IO ()
main =
  getArgs >>= \case
    [imgPath] ->
      getWidth >>= \maxWidth ->
      print maxWidth >>
      putStrLn "Converting image to ASCII..." >>
      getImage imgPath >>= \case
        Left err  -> putStrLn $ "Error: " ++ err
        Right image -> printAscii image maxWidth
    _ ->
      putStrLn "Error: No image input detected." >>
      putStrLn "Drag an image onto the executable to get started!"