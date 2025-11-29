# ASCII to Image - Haskell
This tool converts images to ASCII of a specified width.

It imports the [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels) `Codec.Picture` module to load images as an array of RGBA values. Therefore, it quite effortlessly supports multiple image formats.

I also made a version of this in C++: https://github.com/KojoBailey/image-to-ascii-cpp