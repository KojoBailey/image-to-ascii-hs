{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Vector.Storable as S
import Codec.Picture

data Pixel = RGB Float Float Float | RGBA Float Float Float Float
  deriving (Show)

-- type ImageData = [Pixel]

-- ascii_symbols :: [Char]
-- ascii_symbols = [' ', '.', ':', '-', '=', '+', '/', '%', '#', '@']

-- rgb_to_symbol :: Pixel -> Either String Char
-- rgb_to_symbol (RGBA r g b a) = if index >= 0 && index <= 9
--   then Right $ ascii_symbols !! index
--   else Left "Invalid RGBA values. Keep all between 0-1."
--   where
--     intensity = (r + g + b) / 3 * a
--     index = round $ intensity * (fromIntegral (length ascii_symbols) - 1)

get_width :: IO Int
get_width =
  putStrLn "Enter output width (recommended max 140) or nothing for default (80):" >>
  getLine >>= \input ->
  pure $ if null input then 80 else (read input :: Int)

get_image_data :: String -> IO (Either String (S.Vector (PixelBaseComponent PixelRGBA8)))
get_image_data path = 
  readImage path >>= \result ->
  pure $ case result of
    Left err      -> Left err
    Right dynImg  -> Right $ imageData (convertRGBA8 dynImg)

convert_rec :: S.Vector (PixelBaseComponent PixelRGBA8) -> Int -> [Main.Pixel]
convert_rec v i = RGBA r g b a : convert_rec v (i+4)
  where
    r = fromIntegral (v S.! i) / 255
    g = fromIntegral (v S.! (i+1)) / 255
    b = fromIntegral (v S.! (i+2)) / 255
    a = fromIntegral (v S.! (i+3)) / 255

convert :: S.Vector (PixelBaseComponent PixelRGBA8) -> [Main.Pixel]
convert v = convert_rec v 0 


main :: IO ()
main =
  get_width >>= \width ->
  print width >>
  putStrLn "Converting image to ASCII..." >>
  let imgPath = "C:/Users/kojom/Downloads/7c1b1215-a00a-4ef7-8331-e2dfab6f5af7.jpg" in
  get_image_data imgPath >>= \case
    Left err      -> putStrLn $ "Error: " ++ err
    Right dynImg  -> print $ convert dynImg