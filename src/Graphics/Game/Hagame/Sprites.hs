{-# LANGUAGE FlexibleInstances #-}

module Graphics.Game.Hagame.Sprites where

import RIO
import qualified RIO.List as List
import qualified RIO.HashMap as HM
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
            , sprTexture :: Maybe Texture -- ^ Sprite texture
            , sprTransform :: Transform -- ^ Sprite global transform
            , sprColor :: GL.Color4 Float -- ^ Sprite color hint
            , sprPriority :: Double
            }

-- instance Eq Sprite where


-- instance Ord Sprite where
--     spr1 <= spr2 = (sprPriority spr1) <= (sprPriority spr2)

class HasSpriteSystem env where
    getSpriteSystem :: env -> SpriteSystem

renderSprites :: HasSpriteSystem env => RIO env ()
renderSprites = do
    sprites <- drawingOrder
    mapM_ renderSprite sprites

destroySprites :: HasSpriteSystem env => RIO env ()
destroySprites = do
    sprites <- drawingOrder
    mapM_ deleteSprite sprites

drawingOrder :: HasSpriteSystem env => RIO env [Sprite]
drawingOrder = do
    system <- getSpriteSystem <$> ask

    map <- readIORef $ ssSprites system

    return $ List.sortOn sprPriority $ HM.elems map

addSprite :: HasSpriteSystem env => Sprite -> RIO env Int
addSprite spr = do
    system <- getSpriteSystem <$> ask

    lastId <- readIORef $ ssLastId system
    let newId = lastId + 1

    modifyIORef (ssSprites system) (HM.insert newId spr)
    writeIORef (ssLastId system) newId
    return newId

getSprite :: HasSpriteSystem env => Int -> RIO env Sprite
getSprite id = do
    mSpr <- HM.lookup id <$> ((readIORef . ssSprites . getSpriteSystem) =<< ask)

    case mSpr of 
        (Just spr) -> return spr
        Nothing -> throwString $ "No sprite with id: " <> show id

removeSprite :: HasSpriteSystem env => Int -> RIO env Sprite
removeSprite id = do
    system <- getSpriteSystem <$> ask
    
    spr <- getSprite id

    modifyIORef (ssSprites system) (HM.delete id)

    return spr


data SpriteSystem = 
    SpriteSystem    { ssSprites :: IORef (HM.HashMap Int Sprite)
                    , ssLastId :: IORef Int
                    }


instance HasSpriteSystem SpriteSystem where
    getSpriteSystem = id

emptySpriteSystem :: IO SpriteSystem
emptySpriteSystem = do
    sprites <- newIORef HM.empty
    lastId <- newIORef 0
    return $ SpriteSystem sprites lastId

-- | Create a sprite
createSprite    :: MonadIO m
                => Shader -- ^ The sprite shader
                -> Maybe Texture -- ^ The sprite's texture
                -> Transform -- ^ The sprite's global transform (center origin)
                -> GL.Color4 Float -- ^ The sprite's color hint
                -> m Sprite
createSprite shader mTexture transform color = do
    vao <- createSquareVAO
    return $ Sprite shader vao mTexture transform color 1.0

loadSprite  :: (HasSpriteSystem env, HasShaders env, HasTextures env)
            => String -- ^ Shader name
            -> Maybe String -- ^ Texture name
            -> Transform 
            -> GL.Color4 Float
            -> RIO env Int
loadSprite shaderName mTextureName trans color = do
    state <- ask
    
    shader <- GL.get $ shaderVar state shaderName
    mTexture <- case mTextureName of
        Just textureName -> Just <$> GL.get (textureVar state textureName)
        Nothing -> return Nothing

    sprite <- createSprite shader mTexture trans color
    id <- addSprite sprite

    return id


-- | Draw the sprite to the OpenGL context
renderSprite :: MonadIO m => Sprite -> m ()
renderSprite (Sprite shader vao mTexture transform color _) = do
    model <- toGLMatrix $ getTransformMatrix transform
    
    useShader shader

    uniform shader "model" $= model

    uniform shader "spriteColor" $= color


    GL.activeTexture $= GL.TextureUnit 0
    case mTexture of 
        Just texture -> bindTexture texture
        Nothing -> clearTexture 

    GL.bindVertexArrayObject $= Just vao
    liftIO $ GL.drawArrays GL.Triangles 0 6
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
