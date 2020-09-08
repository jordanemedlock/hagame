module Graphics.Game.Hagame.Texture (
    loadTexture, bindTexture, Texture(texSize)
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


data Texture = 
    Texture { texId :: GL.TextureObject
            , texSize :: GL.Vector2 Int
            }


createTexture :: Int -> Int -> GL.PixelData a -> IO Texture
createTexture width height textureData = do
    texId <- GL.genObjectName :: IO GL.TextureObject

    GL.textureBinding GL.Texture2D $= Just texId

    let size = GL.TextureSize2D (fromIntegral width) (fromIntegral height)
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 size 0 textureData

    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

    GL.textureBinding GL.Texture2D $= Nothing

    return $ Texture texId (GL.Vector2 width height)

bindTexture :: Texture -> IO ()
bindTexture (Texture texId _) = do
    GL.textureBinding GL.Texture2D $= Just texId

loadTexture :: String -> IO (Either String Texture)
loadTexture filename = do
    eimage <- readImage filename

    case eimage of
        Left msg -> do
            putStrLn msg
            return $ Left msg
        Right image -> do
            let rgbPixel = convertRGBA8 image
            
            -- let rgbPixel = convertRGBA8 image
            let width = fromIntegral $ imageWidth rgbPixel
            let height = fromIntegral $ imageHeight rgbPixel
            let idata = imageData rgbPixel :: VS.Vector Word8

            -- print idata

            Right <$> (VS.unsafeWith idata $ \ptr -> 
                createTexture width height (GL.PixelData GL.RGBA GL.UnsignedByte ptr))