{-# LANGUAGE TupleSections #-}

module Graphics.Game.Hagame.Fonts (
      Character(charSize, charBearing, charAdvancement), Font(fontCharacters, fontShader)
    , HasFonts(..)
    , loadFont, renderString
) where

import RIO
import RIO.List.Partial ((!!))
import qualified RIO.HashMap as HM
import FreeType
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
-- import GHC.Int (Int32, Int64)
import Foreign.Storable (peek, sizeOf)
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Array (newArray)
-- import Control.Monad (foldM_)
import Graphics.Game.Hagame.Shader (Shader, useShader, uniform)
import Graphics.Game.Hagame.Utils

-- | 'Character' data type for fonts, contains everthing to render a 'Char'
data Character = 
    Character   { charTexId :: GL.TextureObject -- ^ OpenGL texture id for character
                , charSize :: GL.TextureSize2D -- ^ Size of the character
                , charBearing :: GL.Vector2 Int32 -- ^ Bearing/offset for the character 
                , charAdvancement :: Int32 -- ^ Advancement/kerning for the character
                }

-- | 'Font' data type contains everything to render a string using the font
data Font = 
    Font    { fontCharacters :: [Character] -- ^ A list of 'Character's in ASCII order
            , fontShader :: Shader -- ^ Glyph shader for the font
            , fontVAO :: GL.VertexArrayObject -- ^ Reusable VAO to hold the tris to draw
            , fontVBO :: GL.BufferObject -- ^ Reusable VBO to hold the tris
            }

data FontSystem = 
    FontSystem  { fsFonts :: IORef (HM.HashMap String Font) }

initFontSystem :: IO FontSystem
initFontSystem = FontSystem <$> newIORef HM.empty

class HasFonts e where
    fontVar :: e -> String -> GL.StateVar Font

class HasFontSystem env where
    getFontSystem :: env -> FontSystem

instance HasFontSystem FontSystem where
    getFontSystem = id

instance HasFonts FontSystem where 
    fontVar env = stateVarFromHashMap (fsFonts env)

-- | Load a font into a global env
loadFont    :: HasFonts env
            => String -- ^ Font name
            -> String -- ^ Font filename
            -> Int
            -> Shader
            -> RIO env Font
loadFont name filename height shader = do
    env <- ask

    font <- readFont filename height shader

    fontVar env name $= font

    return font

-- | Read a font from a TTF file 
readFont    :: MonadIO m 
            => String   -- ^ Filename
            -> Int      -- ^ Font size/height in pixels
            -> Shader   -- ^ Glyph shader to render the font
            -> m Font
readFont name height shader = withFontFace name $ \lib face -> do
    GL.rowAlignment GL.Unpack $= 1

    liftIO $ ft_Set_Pixel_Sizes face 0 (fromIntegral height)

    chars <- mapM (loadChar face) [0..127]

    (vao, vbo) <- loadFontVAOVBO

    return $ Font chars shader vao vbo

withFontFace :: MonadIO m 
             => String 
             -> (FT_Library -> FT_Face -> m a)
             -> m a 
withFontFace name f = do
    lib <- liftIO $ ft_Init_FreeType
    face <- liftIO $ ft_New_Face lib name 0 

    ret <- f lib face

    liftIO $ ft_Done_Face face
    liftIO $ ft_Done_FreeType lib

    return ret

-- | Load the VAO and VBO objects and sets them up
loadFontVAOVBO :: MonadIO m => m (GL.VertexArrayObject, GL.BufferObject)
loadFontVAOVBO = do
    vao <- GL.genObjectName 
    vbo <- GL.genObjectName 

    GL.bindVertexArrayObject $= Just vao
    
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.bufferData GL.ArrayBuffer $= (fromIntegral $ sizeOf (0.0 :: Float) * 6 * 4, nullPtr, GL.DynamicDraw)

    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (fromIntegral $ 4 * sizeOf (0.0 :: Float)) nullPtr)

    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.bindVertexArrayObject $= Nothing

    return (vao, vbo)

-- | Load the font into a 'Character' object
loadChar    :: MonadIO m
            => FT_Face -- ^ FreeType font object
            -> Int -- ^ ASCII character code 
            -> m Character
loadChar pFace c = liftIO do
    ft_Load_Char pFace (fromIntegral c) FT_LOAD_RENDER

    texId <- GL.genObjectName

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

-- | Render string to the OpenGL context using the provided font
renderString    :: MonadIO m
                => Font -- ^ Font to used to render string
                -> String -- ^ String to render
                -> GL.Vector2 Float -- ^ Render position
                -> Float -- ^ Scale relative to loaded font size
                -> GL.Color4 Float -- ^ Font color
                -> m ()
renderString font@(Font chars shader vao vbo) string pos scale color = do

    useShader shader

    uniform shader "textColor" $= color 

    GL.activeTexture $= GL.TextureUnit 0
    GL.bindVertexArrayObject $= Just vao

    let (GL.Vector2 x y) = pos

    foldM_ (\x c -> renderCharacter font y scale (chars !! fromEnum c) x) x string

    GL.bindVertexArrayObject $= Nothing
    GL.textureBinding GL.Texture2D $= Nothing


-- | Render character to the screen
renderCharacter :: MonadIO m 
                => Font -- ^ Font used
                -> Float -- ^ y position of the character
                -> Float -- ^ Scale
                -> Character -- ^ Character to render
                -> Float -- ^ x position to render at
                -> m Float -- ^ Advanced x position after render
renderCharacter (Font chars _ vao vbo) y scale (Character tex cSize bear adv) x = do

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
    vertices <- liftIO $ newArray verticesL
    let verticesSize = fromIntegral $ sizeOf (0.0 :: Float) * length verticesL

    GL.textureBinding GL.Texture2D $= Just tex

    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    liftIO $ GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer 0 verticesSize vertices
    GL.bindBuffer GL.ArrayBuffer $= Nothing

    liftIO $ GL.drawArrays GL.Triangles 0 6

    return $ x + (fromIntegral adv/64 * scale)



    