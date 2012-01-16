module Buffers where

import Graphics.Rendering.OpenGL
import Foreign.Storable (Storable(..), sizeOf)
import Foreign.Ptr (Ptr, wordPtrToPtr)
import Data.Array.Storable
import AltPrelude
import Prelude ()


mkBuffer :: (Storable a) => BufferTarget -> [a] -> IO BufferObject
mkBuffer target xs = withNewName (loadBuffer target xs)

loadBuffer :: (Storable a) => BufferTarget -> [a] -> BufferObject -> IO ()
loadBuffer target xs buf = do bindBuffer target $= Just buf
                              arr <- newListArray (0, n - 1) xs
                              withStorableArray arr $ \ptr ->
                                  bufferData target $= (sz, ptr, StaticDraw)
  where n = length xs
        sz = fromIntegral $ n * sizeOfElem xs

withNewName :: (ObjectName n) => (n -> IO a) -> IO n
withNewName f = do [n] <- genObjectNames 1
                   f n
                   return n

sizeOfElem :: (Storable a) => f a -> Int
sizeOfElem xs = sizeOf (fake xs)
  where fake :: f a -> a
        fake _ = undefined

getOffset :: (Storable a) => [a] -> Ptr a
getOffset xs = wordPtrToPtr . fromIntegral $ sizeOfElem xs * length xs

zeroOffset :: Ptr a
zeroOffset = wordPtrToPtr 0

