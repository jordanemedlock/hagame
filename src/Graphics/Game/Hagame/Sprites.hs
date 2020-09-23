module Graphics.Game.Hagame.Sprites (
      Transform(..), Sprite(..)
    , createSprite, renderSprite, deleteSprite, rotateSprite, moveSprite, updateSpritePos
) where

import RIO
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
-- import GHC.Int
-- import Data.Word
import qualified Data.Matrix as M
import Graphics.Game.Hagame.Utils
import Codec.Picture (readImage, generateImage, convertRGBA8, DynamicImage(..), Image(..), PixelRGBA8(..), readPng)
import Codec.Picture.Types (promoteImage)
import qualified Data.Vector.Storable as VS
-- import qualified Data.ByteString as BS
import System.FilePath.Posix (takeExtensions)
import Graphics.Game.Hagame.Texture
import Graphics.Game.Hagame.Shader


-- | Sprite object
data Sprite = 
    Sprite  { sprShader :: Shader -- ^ Shader for rendering
            , sprVAO :: GL.VertexArrayObject -- ^ VAO sprite geometry
            , sprTexture :: Texture -- ^ Sprite texture
            , sprTransform :: Transform -- ^ Sprite global transform
            , sprColor :: GL.Color4 Float -- ^ Sprite color hint
            }


-- | Create a sprite
createSprite    :: Shader -- ^ The sprite shader
                -> Texture -- ^ The sprite's texture
                -> Transform -- ^ The sprite's global transform (center origin)
                -> GL.Color4 Float -- ^ The sprite's color hint
                -> RIO env Sprite
createSprite shader texture transform color = do
    vao <- liftIO createSquareVAO
    return $ Sprite shader vao texture transform color

-- | Draw the sprite to the OpenGL context
renderSprite :: Sprite -> RIO env ()
renderSprite (Sprite shader vao texture transform color) = liftIO do
    model <- toGLMatrix $ getTransformMatrix transform
    
    useShader shader

    uniform shader "model" $= model

    uniform shader "spriteColor" $= color

    GL.activeTexture $= GL.TextureUnit 0
    bindTexture texture

    GL.bindVertexArrayObject $= Just vao
    GL.drawArrays GL.Triangles 0 6
    GL.bindVertexArrayObject $= Nothing


-- | Helper function to rotate the sprite
rotateSprite :: Sprite -> Float -> Sprite
rotateSprite sprite rot = sprite { sprTransform = trans { rotation2D = rot' } }
    where 
        trans = sprTransform sprite
        rot' = rotation2D trans + rot

-- | Helper function to move the sprite
moveSprite :: Sprite -> GL.Vector3 Float -> Sprite
moveSprite sprite (GL.Vector3 dx dy dz) = updateSpritePos sprite (\(GL.Vector3 x y z) -> GL.Vector3 (x + dx) (y + dy) (z + dy))


updateSpritePos :: Sprite -> (GL.Vector3 Float -> GL.Vector3 Float) -> Sprite
updateSpritePos sprite f = sprite { sprTransform = trans { position = f pos }}
    where
        trans = sprTransform sprite
        pos = position trans

-- | Delete the sprite
deleteSprite :: MonadIO m => Sprite -> m ()
deleteSprite = GL.deleteObjectName . sprVAO
