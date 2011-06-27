module Tga (tgaHeader, getAsByteArray) where

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as B
import Tracertypes

toByte :: Float -> Word8
toByte x = round $ x * 255

split16 :: Word16 -> (Word8, Word8)
split16 x = (fromIntegral x, fromIntegral (x `shift` (-8)))

getAsBytes :: Color -> B.ByteString
getAsBytes c = B.pack $ getAsByteArray c

getAsByteArray :: Color -> [Word8]
getAsByteArray (Color r g b) = [toByte b, toByte g, toByte r, 255]

imageSpec :: Word16 -> Word16 -> Word16 -> Word16 -> Word8 -> Word8 -> [Word8]
imageSpec xo yo w h depth imageDescriptor = 
  [xo1, xo2, yo1, yo2, w1, w2, h1, h2, depth, imageDescriptor]
  where (xo1, xo2) = split16 xo
        (yo1, yo2) = split16 yo
        (w1, w2) = split16 w
        (h1, h2) = split16 h

tgaHeader :: Int -> Int -> B.ByteString
tgaHeader w h = B.pack (id ++ colorMapType ++ imageType ++ colorMapSpec ++ imSpec)
   where id = [0]
         colorMapType = [0]
         imageType = [2]
         colorMapSpec = [0,0,0,0,0]
         imSpec = imageSpec 0 0 (fromIntegral w) (fromIntegral h) 32 0