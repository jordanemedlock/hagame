module Graphics.Game.Hagame.Texture (
    loadTexture, bindTexture, Texture(texSize)
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

-- | Texture wrapper
data Texture = 
    Texture { texId :: GL.TextureObject -- ^ OpenGL texture ID
            , texSize :: GL.Vector2 Int -- ^ Texture size
            }

-- | Create a texture using the pixel data
createTexture   :: MonadIO m 
                => Int -- ^ Texture width
                -> Int -- ^ Texture height
                -> GL.PixelData a -- ^ Texture pixel data
                -> m Texture
createTexture width height textureData = do
    texId <- GL.genObjectName

    GL.textureBinding GL.Texture2D $= Just texId

    let size = GL.TextureSize2D (fromIntegral width) (fromIntegral height)
    liftIO $ GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 size 0 textureData

    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

    GL.textureBinding GL.Texture2D $= Nothing

    return $ Texture texId (GL.Vector2 width height)

-- | Binds the texture to the OpenGL context
bindTexture :: MonadIO m => Texture -> m ()
bindTexture (Texture texId _) = do
    GL.textureBinding GL.Texture2D $= Just texId

-- | Loads the texture from a png file
loadTexture :: HasLogFunc env
            => String -- ^ Filename
            -> RIO env Texture
loadTexture filename = do
    eimage <- liftIO $ readImage filename

    case eimage of
        Left msg -> throwString msg
            
        Right image -> do
            let rgbPixel = convertRGBA8 image
            
            let width = fromIntegral $ imageWidth rgbPixel
            let height = fromIntegral $ imageHeight rgbPixel
            let idata = imageData rgbPixel :: VS.Vector Word8

            liftIO $ (VS.unsafeWith idata $ \ptr -> 
                createTexture width height (GL.PixelData GL.RGBA GL.UnsignedByte ptr))