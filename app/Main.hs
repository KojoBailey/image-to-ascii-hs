{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment ( getArgs )
import qualified Data.Vector.Storable as S
import Data.Word ( Word8 )
import qualified Codec.Picture as Juicy
import Foreign.Storable
import Text.Read ( readMaybe )
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

data Pixel = RGBA !Word8 !Word8 !Word8 !Word8
  deriving (Show)

instance Storable Pixel where
  sizeOf _ = 4
  alignment _ = alignment (undefined :: Word8)
  peek ptr = RGBA <$> off 0 <*> off 1 <*> off 2 <*> off 3
    where off = peekByteOff ptr
  poke ptr (RGBA r g b a) =
    let off = pokeByteOff ptr in
    off 0 r >> off 1 g >> off 2 b >> off 3 a

type Pixels = S.Vector Pixel

data Image = Image {
  pixels :: Pixels,
  width  :: Int,
  height :: Int }

-- Clamping

clamp :: Int -> Image -> Image
clamp newWidth image = Image {
    pixels = S.generate newSize clampStep,
    width  = newWidth,
    height = newHeight }
  where
    newSize   = newWidth * newHeight
    newHeight = round (fromIntegral (height image) / xStepSize / 2)
    xStepSize = fromIntegral (width image) / fromIntegral newWidth :: Float
    yStepSize = fromIntegral (height image) / fromIntegral newHeight :: Float

    clampStep :: Int -> Pixel
    clampStep i = pixels image `S.unsafeIndex` index
      where
        index  = yIndex * width image + xIndex
        yIndex = clampIndex (height image) (fromIntegral row * yStepSize)
        xIndex = clampIndex (width image) (fromIntegral column * xStepSize)
        row    = i `div` newWidth :: Int
        column = i `mod` newWidth :: Int

        clampIndex :: Int -> Float -> Int
        clampIndex dimensionMax normalIndex = min (dimensionMax - 1) (floor normalIndex)

-- Conversion

convert :: Juicy.Image Juicy.PixelRGBA8 -> Image
convert image = Image {
  pixels = S.generate size convertStep,
  width  = imageWidth,
  height = imageHeight }
  where
    size        = imageWidth * imageHeight
    imageWidth  = Juicy.imageWidth image
    imageHeight = Juicy.imageHeight image

    getImageByte :: Int -> Word8
    getImageByte i = fromIntegral $ imageData `S.unsafeIndex` i
      where imageData = Juicy.imageData image

    convertStep :: Int -> Pixel
    convertStep i = RGBA (get 0) (get 1) (get 2) (get 3)
      where get off = getImageByte $ i * 4 + off

-- ASCII

asciiSymbols :: [Char]
asciiSymbols = [' ', '.', ':', '-', '=', '+', '/', '%', '#', '@']

toAscii :: Pixel -> Char
toAscii (RGBA r g b a) = asciiSymbols !! round (lightness * fromIntegral maxIndex)
  where
    lightness = (mn + mx) / 2 / 255 * opacity :: Float
    mn        = fromIntegral $ minimum [r, g, b] :: Float
    mx        = fromIntegral $ maximum [r, g, b] :: Float
    opacity   = fromIntegral a / 255
    maxIndex  = length asciiSymbols - 1

-- Printing

generateString :: Int -> (Int -> TLB.Builder) -> TL.Text
generateString n f = TLB.toLazyText $ foldMap f [0..n-1]

printAscii :: Juicy.Image Juicy.PixelRGBA8 -> Int -> IO ()
printAscii originalImage maxWidth = TLIO.putStrLn $ generateString size printAsciiStep
  where
    size       = S.length imageAscii
    imageAscii = S.map toAscii (pixels image)
    image      = clamp newWidth (convert originalImage)
    newWidth   = min maxWidth (Juicy.imageWidth originalImage)

    printAsciiStep :: Int -> TLB.Builder
    printAsciiStep i =
      if (i+1) `mod` newWidth == 0
        then c <> TLB.fromString "\n"
        else c
      where c = TLB.singleton $ imageAscii `S.unsafeIndex` i

-- User input

getWidth :: IO Int
getWidth = do
  putStrLn $ "Enter output width or nothing for default (" ++ show default_  ++ "):"
  go
  where
    default_ = 80
    go = do
      input <- getLine
      case input of
        "" -> pure default_
        _  -> case readMaybe input :: Maybe Int of
          Just w  -> pure w
          Nothing -> putStrLn "Invalid width. Try again:" >> go

getImage :: String -> IO (Either String (Juicy.Image Juicy.PixelRGBA8))
getImage path = fmap Juicy.convertRGBA8 <$> Juicy.readImage path

main :: IO ()
main =
  getArgs >>= \case
    [imgPath] -> do
      maxWidth <- getWidth
      putStrLn "Converting image to ASCII..."
      getImage imgPath >>= \case
        Left err  -> putStrLn $ "Error: " ++ err
        Right image -> printAscii image maxWidth
    _ -> do
      putStrLn "Error: No image input detected."
      putStrLn "Drag an image onto the executable to get started!"