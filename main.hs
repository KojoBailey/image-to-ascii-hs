import Control.Monad

data Pixel = RGB Float Float Float | RGBA Float Float Float Float

ascii_symbols :: [Char]
ascii_symbols = [' ', '.', ':', '-', '=', '+', '/', '%', '#', '@']

rgb_to_symbol :: Pixel -> Maybe Char
rgb_to_symbol (RGBA r g b a) = ascii_symbols !! index <$ guard (index >= 0 && index <= 9)
  where
    intensity = (r + g + b) / 3 * a
    index = round $ intensity * (fromIntegral (length ascii_symbols) - 1)

get_width :: IO Int
get_width =
  putStrLn "Enter output width (recommended max 140) or nothing for default (80):" >>
  getLine >>= \input ->
  pure $ if null input then 80 else (read input :: Int)

main :: IO ()
main =
  get_width >>= \width ->
  print width >>
  putStrLn "Converting image to ASCII..."