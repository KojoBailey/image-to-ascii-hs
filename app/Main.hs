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

clamp_x_rec :: Int -> Float -> Int -> Int -> Float -> Image -> Pixels
clamp_x_rec column step new_width y_index step_size img
  | column >= new_width = S.empty
  | otherwise       =
    pixels img `S.unsafeIndex` (y_index * width img + x_index)
      `S.cons` clamp_x_rec (column+1) (step + step_size) new_width y_index step_size img
  where
    x_index = if (column+1) == new_width then width img - 1 else floor step

clamp_x :: Int -> Int -> Float -> Image -> Pixels
clamp_x = clamp_x_rec 0 0

clamp_y_rec :: Int -> Float -> Int -> Int -> Float -> Float -> Image -> Pixels
clamp_y_rec row step new_height new_width y_step_size x_step_size img
  | row >= new_height = S.empty
  | otherwise     =
    clamp_x new_width y_index x_step_size img
      S.++ clamp_y_rec (row+1) (step + y_step_size) new_height new_width y_step_size x_step_size img
  where
    y_index = if (row+1) == new_height then height img - 1 else floor step

clamp_y :: Int -> Int -> Float -> Float -> Image -> Image
clamp_y new_height new_width y_step_size x_step_size img =
  Image {
    pixels = clamp_y_rec 0 0 new_height new_width y_step_size x_step_size img,
    width = new_width,
    height = new_height }

clamp :: Int -> Image -> Image
clamp max_width img =
  clamp_y new_height new_width y_step_size x_step_size img
  where
    new_width = min max_width (width img)
    new_height = round (fromIntegral (height img) / x_step_size / 2)
    x_step_size = fromIntegral (width img) / fromIntegral new_width
    y_step_size = fromIntegral (height img) / fromIntegral new_height

clamp2 :: Int -> Image -> Image
clamp2 maxWidth image = Image {
    pixels = S.generate newSize (\i -> RGBA 255 255 255 255),
    width = newWidth,
    height = newHeight }
  where
    newSize = newWidth * newHeight
    newWidth = min maxWidth (width image)
    newHeight = round (fromIntegral (height image) / xStepSize / 2)
    xStepSize = fromIntegral (width image) / fromIntegral newWidth :: Float
    yStepSize = fromIntegral (height image) / fromIntegral newHeight :: Float

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
    img = clamp2 max_width (convert orig_img)
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