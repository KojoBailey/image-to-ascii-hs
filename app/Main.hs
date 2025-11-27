{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad

import Codec.Picture
import Codec.Picture.Types (pixelFold)

-- data Pixel = RGB Float Float Float | RGBA Float Float Float Float

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

main :: IO ()
main =
  get_width >>= \width ->
  print width >>
  putStrLn "Converting image to ASCII..." >>
  let imgPath = "C:/Users/kojom/Downloads/7c1b1215-a00a-4ef7-8331-e2dfab6f5af7.jpg" in
  readImage imgPath >>= \case
    Left err -> putStrLn $ "Error: " ++ err
    Right dynImg ->
      let rgbaImg = convertRGBA8 dynImg in
      print $ imageData rgbaImg