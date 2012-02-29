module Sandbox.Image.BMP where

import qualified Data.ByteString as BS
import Codec.BMP

mkTrivialBitmap width height pixels 
  = packRGBA32ToBMP width height
  $ BS.pack $ take (width * height * 4)
  $ concat [ [r,g,b,0]
           | (r,g,b) <- pixels ]
    ++ repeat 0

trivialBitmapTest = writeBMP "test.bmp"
                  $ mkTrivialBitmap 2 2
                    [ (10, 20, 55)
                    , ( 4, 10, 12)
                    , (40,  1,  0)
                    ]

main = trivialBitmapTest