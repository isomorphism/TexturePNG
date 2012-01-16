module Texture where

import Data.Word
import Graphics.Rendering.OpenGL hiding (imageHeight)
import Codec.Picture.Repa
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as VA
import Foreign.Ptr
import qualified Data.Vector.Storable as S
import qualified Data.Array.Repa as R
import Data.Array.Repa (Z(..), (:.)(..))
import Buffers
import AltPrelude
import Prelude ()


getTexture fn = either error id <$> loadTextureRGBA fn

loadTextureRGBA fn = eitherF return imgToTexture =<< readImageRGBA fn


fixDims :: R.DIM3 -> R.DIM3
fixDims (Z :. x :. y :. z) = Z :. y :. x :. z

mapDims fx fy fz (Z :. x :. y :. z) = Z :. fx x :. fy y :. fz z

flipDims x1 x2 x3 ar = R.traverse ar id (. mapDims (f x1 m1) (f x2 m2) (f x3 m3))
  where (Z :. m1 :. m2 :. m3) = R.arrayExtent ar
        f True  m x = m - x - 1
        f False _ x = x

genTranspose :: (R.Shape sh, R.Elt a) => (sh -> sh) -> R.Array sh a -> R.Array sh a
genTranspose tr array = R.traverse array tr (. tr)

imgSize :: Img a -> (Int, Int)
imgSize img = let (Z :. w :. h :. _) = R.arrayExtent (imgData img) 
              in (w, h)

imgToTexture img = mkTexture (imgSize img) . S.convert . R.toVector $ ar
  where ar = flipDims False True True . genTranspose fixDims $ imgData img

mkTexture :: (Int, Int) -> S.Vector Word8 -> IO TextureObject
mkTexture sz pixels = withNewName (loadTexture sz pixels)

loadTexture :: (Int, Int) -> S.Vector Word8 -> TextureObject -> IO ()
loadTexture sz pixels tx = do textureBinding Texture2D $= Just tx
                              S.unsafeWith pixels (loadPixels sz)
                              textureFilter Texture2D $= ((Linear', Nothing), Linear')
                              textureWrapMode Texture2D S $= (Repeated, Repeat)
                              textureWrapMode Texture2D T $= (Repeated, Repeat)

loadPixels :: (Int, Int) -> Ptr Word8 -> IO ()
loadPixels (w, h) pxs = loadPx pxs
  where sz = TextureSize2D (fromIntegral w) (fromIntegral h)
        loadPx = texImage2D Nothing NoProxy 0 RGBA' sz 0 
               . PixelData RGBA VA.UnsignedByte


