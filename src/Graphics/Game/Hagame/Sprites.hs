module Graphics.Game.Hagame.Sprites (
    Transform(..), Sprite(..), createSprite, drawSprite, deleteSprite, rotateSprite
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

data Transform = 
    Transform2D { position2D :: GL.Vector2 Float 
                , size2D :: GL.Vector2 Float
                , rotation2D :: Float
                }


data Sprite = 
    Sprite  { sprShader :: Shader
            , sprVAO :: GL.VertexArrayObject
            , sprTexture :: Texture
            , sprTransform :: Transform
            , sprColor :: GL.Color4 Float
            }

createSpriteVAO :: IO GL.VertexArrayObject
createSpriteVAO = do
    
    let verticesL = [ (-0.5), (-0.5),   0.0, 1.0
                    , 0.5, 0.5,         1.0, 0.0
                    , (-0.5), 0.5,      0.0, 0.0
                    
                    , (-0.5), (-0.5),   0.0, 1.0
                    , 0.5, (-0.5),      1.0, 1.0
                    , 0.5, 0.5,         1.0, 0.0
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

createSprite :: Shader -> Texture -> Transform -> GL.Color4 Float -> IO Sprite
createSprite shader texture transform color = do
    vao <- createSpriteVAO
    return $ Sprite shader vao texture transform color

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

getTransformMatrix :: Transform -> M.Matrix Float
getTransformMatrix (Transform2D (GL.Vector2 x y) (GL.Vector2 width height) rot) = model
    where 
        translate = translationMat (GL.Vector3 x y 0.0)
        rotate = rotationMat2D (pi / 180 * rot)
        scale = scaleMat (GL.Vector3 width height 1.0)
        model = translate * rotate * scale

rotateSprite :: Sprite -> Float -> Sprite
rotateSprite sprite rot = sprite { sprTransform = trans { rotation2D = rot' } }
    where 
        trans = sprTransform sprite
        rot' = rotation2D trans + rot

deleteSprite :: Sprite -> IO ()
deleteSprite = GL.deleteObjectName . sprVAO
