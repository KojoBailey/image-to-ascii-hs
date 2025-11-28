{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Vector.Storable as S
import Control.Monad
import Codec.Picture

data Pixel = RGBA Float Float Float Float
  deriving (Show)

min3 :: Ord a => a -> a -> a -> a
min3 a b = min (min a b)

max3 :: Ord a => a -> a -> a -> a
max3 a b = max (max a b)

ascii_symbols :: [Char]
ascii_symbols = [' ', '.', ':', '-', '=', '+', '/', '%', '#', '@']

to_ascii :: Main.Pixel -> Char
to_ascii (RGBA r g b a) = ascii_symbols !! index
  where
    lightness = (min3 r g b + max3 r g b) / 2 * a
    index = round (lightness * fromIntegral (length ascii_symbols - 1))

get_width :: IO Int
get_width =
  putStrLn "Enter output width (recommended max 140) or nothing for default (80):" >>
  getLine >>= \input ->
  pure $ if null input then 80 else (read input :: Int)

get_image :: String -> IO (Either String (Image PixelRGBA8))
get_image path = 
  readImage path >>= \result ->
  pure $ case result of
    Left err      -> Left err
    Right dynImg  -> Right $ convertRGBA8 dynImg

convert_rec :: Int -> S.Vector (PixelBaseComponent PixelRGBA8) -> [Main.Pixel]
convert_rec i v = RGBA r g b a : convert_rec (i+4) v
  where
    r = fromIntegral (v S.! i) / 255
    g = fromIntegral (v S.! (i+1)) / 255
    b = fromIntegral (v S.! (i+2)) / 255
    a = fromIntegral (v S.! (i+3)) / 255

convert :: S.Vector (PixelBaseComponent PixelRGBA8) -> [Main.Pixel]
convert = convert_rec 0

generate :: [Main.Pixel] -> [Char]
generate = map to_ascii

print_ascii_rec :: Int -> Int -> [Char] -> IO ()
print_ascii_rec _ _ [] = pure ()
print_ascii_rec i w (c:cs) =
  putChar c >>
  when (i > 0 && (i+1) `mod` w == 0) (putChar '\n') >>
  print_ascii_rec (i+1) w cs


print_ascii :: Image PixelRGBA8 -> IO ()
print_ascii img = print_ascii_rec 0 (imageWidth img) ascii
  where ascii = generate . convert . imageData $ img

main :: IO ()
main =
  get_width >>= \width ->
  print width >>
  putStrLn "Converting image to ASCII..." >>
  let imgPath = "C:/Users/kojom/Downloads/7c1b1215-a00a-4ef7-8331-e2dfab6f5af7.jpg" in
  get_image imgPath >>= \case
    Left err  -> putStrLn $ "Error: " ++ err
    Right img -> print_ascii img