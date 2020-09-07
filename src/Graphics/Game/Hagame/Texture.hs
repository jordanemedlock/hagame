module Graphics.Game.Hagame.Texture (
    loadTexture, bindTexture, Texture
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


data Texture = Texture GL.TextureObject


createTexture :: Int32 -> Int32 -> GL.PixelData a -> IO Texture
createTexture width height textureData = do
    texId <- GL.genObjectName :: IO GL.TextureObject

    GL.textureBinding GL.Texture2D $= Just texId

    putStrLn "Setting Texture Image"
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D width height) 0 textureData

    putStrLn "Setting Texture Wrap Mode"
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

    GL.textureBinding GL.Texture2D $= Nothing

    return $ Texture texId

bindTexture :: Texture -> IO ()
bindTexture (Texture texId) = do
    GL.textureBinding GL.Texture2D $= Just texId

loadTexture :: String -> IO Texture -- the a is just a type I dont know yet
loadTexture filename = do
    Right image <- readImage filename

    let rgbPixel = case image of
            ImageRGBA8 img -> promoteImage img :: Image PixelRGBA8
    
    -- let rgbPixel = convertRGBA8 image
    let width = fromIntegral $ imageWidth rgbPixel
    let height = fromIntegral $ imageHeight rgbPixel
    let idata = imageData rgbPixel :: VS.Vector Word8

    -- print idata

    VS.unsafeWith idata $ \ptr -> 
        createTexture width height (GL.PixelData GL.RGBA GL.UnsignedByte ptr)