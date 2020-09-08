module Graphics.Game.Hagame.Sprites (
      Transform(..), Sprite(..)
    , createSprite, drawSprite, deleteSprite, rotateSprite, moveSprite
) where


import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Int
import Data.Word
import qualified Data.Matrix as M
import Graphics.Game.Hagame.Utils
import Codec.Picture (readImage, generateImage, convertRGBA8, DynamicImage(..), Image(..), PixelRGBA8(..), readPng)
import Codec.Picture.Types (promoteImage)
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as BS
import System.FilePath.Posix (takeExtensions)
import Graphics.Game.Hagame.Texture
import Graphics.Game.Hagame.Shader

-- | Transform object
data Transform = 
    Transform   { position :: GL.Vector3 Float -- ^ Position
                , size :: GL.Vector3 Float -- ^ Size
                , rotation2D :: Float -- ^ Rotation
                }

-- | Sprite object
data Sprite = 
    Sprite  { sprShader :: Shader -- ^ Shader for rendering
            , sprVAO :: GL.VertexArrayObject -- ^ VAO sprite geometry
            , sprTexture :: Texture -- ^ Sprite texture
            , sprTransform :: Transform -- ^ Sprite global transform
            , sprColor :: GL.Color4 Float -- ^ Sprite color hint
            }

-- | Creates the sprites VAO and adds the goemetry
createSpriteVAO :: IO GL.VertexArrayObject
createSpriteVAO = do
    
    let verticesL = [ (-0.5), (-0.5),   0.0, 0.0
                    , 0.5, 0.5,         1.0, 1.0
                    , (-0.5), 0.5,      0.0, 1.0
                    
                    , (-0.5), (-0.5),   0.0, 0.0
                    , 0.5, (-0.5),      1.0, 0.0
                    , 0.5, 0.5,         1.0, 1.0
                    ] :: [Float]
    vertices <- newArray verticesL
    let verticesSize = fromIntegral $ sizeOf (0.0 :: Float) * length verticesL

    vao <- GL.genObjectName :: IO GL.VertexArrayObject
    vbo <- GL.genObjectName :: IO GL.BufferObject

    GL.bindVertexArrayObject $= Just vao

    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.bufferData GL.ArrayBuffer $= (verticesSize, vertices, GL.StaticDraw)

    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (fromIntegral $ 4 * sizeOf (0.0 :: Float)) nullPtr)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.bindVertexArrayObject $= Nothing
    GL.deleteObjectName vbo

    return vao

-- | Create a sprite
createSprite    :: Shader -- ^ The sprite shader
                -> Texture -- ^ The sprite's texture
                -> Transform -- ^ The sprite's global transform
                -> GL.Color4 Float -- ^ The sprite's color hint
                -> IO Sprite
createSprite shader texture transform color = do
    vao <- createSpriteVAO
    return $ Sprite shader vao texture transform color

-- | Draw the sprite to the OpenGL context
drawSprite :: Sprite -> IO ()
drawSprite (Sprite shader vao texture transform color) = do
    model <- toGLMatrix $ getTransformMatrix transform
    
    useShader shader

    uniform shader "model" $= model

    uniform shader "spriteColor" $= color

    GL.activeTexture $= GL.TextureUnit 0
    bindTexture texture

    GL.bindVertexArrayObject $= Just vao
    GL.drawArrays GL.Triangles 0 6
    GL.bindVertexArrayObject $= Nothing


-- | Create a 4x4 transformation matrix from Transform object
getTransformMatrix :: Transform -> M.Matrix Float
getTransformMatrix (Transform pos size rot) = model
    where 
        translate = translationMat pos
        rotate = rotationMat2D (pi / 180 * rot)
        scale = scaleMat size
        model = translate * rotate * scale

-- | Helper function to rotate the sprite
rotateSprite :: Sprite -> Float -> Sprite
rotateSprite sprite rot = sprite { sprTransform = trans { rotation2D = rot' } }
    where 
        trans = sprTransform sprite
        rot' = rotation2D trans + rot

-- | Helper function to move the sprite
moveSprite :: Sprite -> GL.Vector3 Float -> Sprite
moveSprite sprite (GL.Vector3 dx dy dz) = sprite { sprTransform = trans { position = pos } }
    where
        trans = sprTransform sprite
        (GL.Vector3 x y z) = position trans
        pos = (GL.Vector3 (x + dx) (y + dy) (z + dz))

-- | Delete the sprite
deleteSprite :: Sprite -> IO ()
deleteSprite = GL.deleteObjectName . sprVAO
