module Main where

import Control.Exception (bracket_)
import GHC.Word
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Events
import Graphics.Rendering.OpenGL

import Texture
import Buffers
import AltPrelude
import Prelude ()

main :: IO ()
main = do bracket_ (initDisplay 500 500) SDL.quit runMainLoop

initDisplay :: Integer -> Integer -> IO ()
initDisplay w h = do SDL.init [SDL.InitEverything]
                     SDL.setVideoMode (fromInteger w) (fromInteger h) 32 [SDL.OpenGL]
                     initGL w h

runMainLoop :: IO ()
runMainLoop = do vb <- mkBuffer ArrayBuffer vertexBufferData
                 tx <- getTexture "test.png"
                 iterWhileM_ (not . any (== Quit)) (mainLoop (vb, tx))

newEvents :: IO [Event]
newEvents = iterWhileM (/= NoEvent) SDL.pollEvent

mainLoop :: (BufferObject, TextureObject) -> IO [Event]
mainLoop x = display x >> SDL.delay 20 >> newEvents

initGL :: Integer -> Integer -> IO ()
initGL w h = do lighting $= Enabled
                texture Texture2D $= Enabled
                cullFace $= Just Back
                colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
                clearColor $= Color4 0 0 0 0
                clearStencil $= 0
                clearDepth $= 1
                depthFunc $= Just Lequal
                initLights
                viewport $= (Position 0 0, Size (fromInteger w) (fromInteger h))
                setCamera 5 aspect 4
  where aspect = fromInteger w / fromInteger h

initLights :: IO ()
initLights = do let l1 = Light 1
                ambient l1 $= Color4 0.2 0.2 0.2 1.0
                diffuse l1 $= Color4 0.9 0.9 0.9 1.0
                specular l1 $= Color4 1.0 1.0 1.0 1.0
                position l1 $= Vertex4 5 5 (-5) 1
                light l1 $= Enabled

setCamera w aspect d = do matrixMode $= Modelview 0
                          loadIdentity
                          frustum (-w/2) (w/2) (-h/2) (h/2) d (d+w)
                          translate $ Vector3 0 0 (- (d + w/2))
  where h = w / aspect

display :: (BufferObject, TextureObject) -> IO ()
display x = do clear [ColorBuffer, DepthBuffer, StencilBuffer]
               preservingMatrix (applyTrans >> displayArrs x)
               SDL.glSwapBuffers

applyTrans = do t <- SDL.getTicks
                translate $ Vector3 0 0 (0 :: GLfloat)
                rotate (camRotX t) $ Vector3 1 0 0
                rotate (camRotY t) $ Vector3 0 1 0
  where camRotX, camRotY :: Word32 -> GLfloat
        camRotX = (* 360) . (/ 10000) . fromIntegral . (`mod` 10000)
        camRotY = (* 360) . (/ 7000) . fromIntegral . (`mod` 7000)

displayArrs (vb, tx) = do textureBinding Texture2D $= Just tx
                          bindBuffer ArrayBuffer $= Just vb
                          clientState NormalArray $= Enabled
                          clientState TextureCoordArray $= Enabled
                          clientState VertexArray $= Enabled
                          arrayPointer NormalArray $= descrNorms
                          arrayPointer TextureCoordArray $= descrTxs
                          arrayPointer VertexArray $= descrVerts
                          drawArrays Quads 0 24
                          clientState NormalArray $= Disabled
                          clientState TextureCoordArray $= Disabled
                          clientState VertexArray $= Disabled
                          bindBuffer ArrayBuffer $= Nothing

vertices, normals, txCds :: [[[GLfloat]]]
vertices = [ [[1, 1, 1], [-1, 1, 1], [-1, -1, 1], [1, -1, 1]]
           , [[1, 1, 1], [1, -1, 1], [1, -1, -1], [1, 1, -1]]
           , [[1, 1, 1], [1, 1, -1], [-1, 1, -1], [-1, 1, 1]]
           , [[-1, 1, 1], [-1, 1, -1], [-1, -1, -1], [-1, -1, 1]]
           , [[-1, -1, -1], [1, -1, -1], [1, -1, 1], [-1, -1, 1]]
           , [[1, -1, -1], [-1, -1, -1], [-1, 1, -1], [1, 1, -1]]
           ]

normals = replicate 4 
      <$> [[0, 0, 1], [1, 0, 0], [0, 1, 0], [-1, 0, 0], [0, -1, 0], [0, 0, -1]]

txCds = replicate 6 $ [ [0, 0], [1, 0], [1, 1], [0, 1] ]

vertexData = concat $ concat vertices
normalData = concat $ concat normals
txCdData = concat $ concat txCds

vertexBufferData = concat [vertexData, normalData, txCdData]

descrVerts = VertexArrayDescriptor 3 Float 0 zeroOffset
descrNorms = VertexArrayDescriptor 3 Float 0 (getOffset vertexData)
descrTxs = VertexArrayDescriptor 2 Float 0 (getOffset $ vertexData ++ normalData)

