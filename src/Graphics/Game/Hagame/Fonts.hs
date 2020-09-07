{-# LANGUAGE TupleSections #-}

module Graphics.Game.Hagame.Fonts where

import FreeType
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import GHC.Int (Int32, Int64)
import Foreign.Storable (peek, sizeOf)
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Array (newArray)
import Control.Monad (foldM_)
import Graphics.Game.Hagame.Shader (Shader, useShader, uniform)

data Character = 
    Character   { charTexId :: GL.TextureObject
                , charSize :: GL.TextureSize2D
                , charBearing :: GL.Vector2 Int32
                , charAdvancement :: Int32
                }

data Font = 
    Font    { fontCharacters :: [Character]
            , fontShader :: Shader
            , fontVAO :: GL.VertexArrayObject
            , fontVBO :: GL.BufferObject
            }

loadFont :: String -> Int -> Shader -> IO Font
loadFont name height shader = ft_With_FreeType $ \ft -> 
    ft_With_Face ft name 0 $ \face -> do

        GL.rowAlignment GL.Unpack $= 1

        ft_Set_Pixel_Sizes face 0 (fromIntegral height)

        chars <- mapM (loadChar face) [0..127]

        (vao, vbo) <- loadFontVAOVBO

        return $ Font chars shader vao vbo


loadFontVAOVBO :: IO (GL.VertexArrayObject, GL.BufferObject)
loadFontVAOVBO = do
    vao <- GL.genObjectName :: IO GL.VertexArrayObject
    vbo <- GL.genObjectName :: IO GL.BufferObject

    GL.bindVertexArrayObject $= Just vao
    
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.bufferData GL.ArrayBuffer $= (fromIntegral $ sizeOf (0.0 :: Float) * 6 * 4, nullPtr, GL.DynamicDraw)

    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (fromIntegral $ 4 * sizeOf (0.0 :: Float)) nullPtr)

    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.bindVertexArrayObject $= Nothing

    return (vao, vbo)

loadChar :: FT_Face -> Int -> IO Character
loadChar pFace c = do
    ft_Load_Char pFace (fromIntegral c) FT_LOAD_RENDER

    texId <- GL.genObjectName :: IO GL.TextureObject

    GL.textureBinding GL.Texture2D $= Just texId

    face <- peek pFace
    glyph <- peek (frGlyph face)
    let bitmap = gsrBitmap glyph

    let pixelData = GL.PixelData GL.Red GL.UnsignedByte (bBuffer bitmap)
    let textureSize = GL.TextureSize2D (fromIntegral $ bWidth bitmap) (fromIntegral $ bRows bitmap)

    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.R8 textureSize 0 pixelData

    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

    return $ Character texId textureSize (GL.Vector2 (gsrBitmap_left glyph) (gsrBitmap_top glyph)) (fromIntegral $ vX $ gsrAdvance glyph)


renderString :: Font -> String -> GL.Vector2 Float -> Float -> GL.Color4 Float -> IO ()
renderString font@(Font chars shader vao vbo) string pos scale color = do

    useShader shader

    uniform shader "textColor" $= color 

    GL.activeTexture $= GL.TextureUnit 0
    GL.bindVertexArrayObject $= Just vao

    let (GL.Vector2 x y) = pos

    foldM_ (\x c -> renderCharacter font y scale color (chars !! fromEnum c) x) x string

    GL.bindVertexArrayObject $= Nothing
    GL.textureBinding GL.Texture2D $= Nothing

renderCharacter :: Font -> Float -> Float -> GL.Color4 Float -> Character -> Float -> IO Float
renderCharacter (Font chars _ vao vbo) y scale color (Character tex cSize bear adv) x = do

    let (GL.TextureSize2D sX' sY') = cSize
    let (sX, sY) = (fromIntegral sX', fromIntegral sY')
    let (GL.Vector2 bX' bY') = bear
    let (bX, bY) = (fromIntegral bX', fromIntegral bY')

    let xpos = x + bX * scale
    let ypos = y - (bY) * scale

    let w = sX * scale
    let h = sY * scale

    let verticesL = [ xpos, ypos + h,   0.0, 1.0
                    , xpos, ypos,       0.0, 0.0
                    , xpos + w, ypos,   1.0, 0.0

                    , xpos, ypos + h,   0.0, 1.0
                    , xpos + w, ypos,   1.0, 0.0
                    , xpos + w, ypos + h, 1.0, 1.0
                    ]
    vertices <- newArray verticesL
    let verticesSize = fromIntegral $ sizeOf (0.0 :: Float) * length verticesL

    GL.textureBinding GL.Texture2D $= Just tex

    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer 0 verticesSize vertices
    GL.bindBuffer GL.ArrayBuffer $= Nothing

    GL.drawArrays GL.Triangles 0 6

    return $ x + (fromIntegral adv/64 * scale)



    