{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment
import qualified Data.Vector.Storable as S
import Data.Word ( Word8 )
import Control.Monad
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
  width :: Int,
  height :: Int
  }

min3 :: Ord a => a -> a -> a -> a
min3 a b = min (min a b)

max3 :: Ord a => a -> a -> a -> a
max3 a b = max (max a b)

ascii_symbols :: [Char]
ascii_symbols = [' ', '.', ':', '-', '=', '+', '/', '%', '#', '@']

to_ascii :: Pixel -> Char
to_ascii (RGBA r g b a) = ascii_symbols !! index
  where
    mn = fromIntegral $ min3 r g b :: Float
    mx = fromIntegral $ max3 r g b :: Float
    lightness = (mn + mx) / 2 / 255 * fromIntegral a / 255 :: Float
    index = round (lightness * fromIntegral maxIndex)
    maxIndex = length ascii_symbols - 1

clamp :: Int -> Image -> Image
clamp maxWidth image = Image {
    pixels = S.generate newSize clampStep,
    width = newWidth,
    height = newHeight }
  where
    newSize = newWidth * newHeight
    newWidth = min maxWidth (width image)
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

convert_rec :: Int -> S.Vector (Juicy.PixelBaseComponent Juicy.PixelRGBA8) -> Pixels
convert_rec i v
  | i >= S.length v     = S.empty
  | otherwise           = RGBA r g b a `S.cons` convert_rec (i+4) v
  where
    r = fromIntegral $ v `S.unsafeIndex` i
    g = fromIntegral $ v `S.unsafeIndex` (i+1)
    b = fromIntegral $ v `S.unsafeIndex` (i+2)
    a = fromIntegral $ v `S.unsafeIndex` (i+3)

convert :: Juicy.Image Juicy.PixelRGBA8 -> Image
convert img = Image {
  pixels = convert_rec 0 (Juicy.imageData img),
  width = Juicy.imageWidth img,
  height = Juicy.imageHeight img }

print_ascii_rec :: Int -> Int -> S.Vector Char -> IO ()
print_ascii_rec i w cs
  | i >= S.length cs = pure ()
  | otherwise            =
    putChar (cs `S.unsafeIndex` i) >>
    when (i > 0 && (i+1) `mod` w == 0) (putChar '\n') >>
    print_ascii_rec (i+1) w cs

print_ascii :: Juicy.Image Juicy.PixelRGBA8 -> Int -> IO ()
print_ascii orig_img max_width = print_ascii_rec 0 (width img) ascii
  where
    img = clamp max_width (convert orig_img)
    ascii = S.map to_ascii (pixels img)

get_width :: IO Int
get_width =
  putStrLn "Enter output width (recommended max 140) or nothing for default (80):" >>
  getLine >>= \input ->
  pure $ if null input then 80 else (read input :: Int)

get_image :: String -> IO (Either String (Juicy.Image Juicy.PixelRGBA8))
get_image path = 
  Juicy.readImage path >>= \result ->
  pure $ case result of
    Left err      -> Left err
    Right dynImg  -> Right $ Juicy.convertRGBA8 dynImg

main :: IO ()
main =
  getArgs >>= \case
    [imgPath] ->
      get_width >>= \max_width ->
      print max_width >>
      putStrLn "Converting image to ASCII..." >>
      get_image imgPath >>= \case
        Left err  -> putStrLn $ "Error: " ++ err
        Right img -> print_ascii img max_width
    _ ->
      putStrLn "Error: No image input detected." >>
      putStrLn "Drag an image onto the executable to get started!"