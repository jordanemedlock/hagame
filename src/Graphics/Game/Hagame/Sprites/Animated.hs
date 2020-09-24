{-# LANGUAGE TupleSections #-}

module Graphics.Game.Hagame.Sprites.Animated where

import RIO
import RIO.List.Partial ((!!), head)
import Graphics.Game.Hagame.Sprites
import Graphics.Game.Hagame.Shader
import Graphics.Game.Hagame.Texture
import Graphics.Game.Hagame.Utils
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Text.Printf
import Data.Either

data Animation = 
    Animation   { animTextures :: [Texture]
                , animLoop :: Bool
                , animDuration :: Float
                , animCurrentTime :: Float
                }

data AnimatedSprite = 
    AnimatedSprite  { asprAnimations :: Map String Animation
                    , asprCurrentAnimation :: Maybe String
                    , asprShader :: Shader
                    , asprVAO :: GL.VertexArrayObject
                    , asprTransform :: Transform
                    , asprColor :: GL.Color4 Float
                    }

createAnimation :: HasLogFunc env => String -> Int -> Float -> Bool -> RIO env Animation
createAnimation filePattern num duration loop = do
    textures <- mapM (loadTexture.(printf filePattern)) [0..num-1]

    return (Animation textures loop duration 0.0)

createAnimatedSprite :: HasLogFunc env => [(String, String, Int, Float, Bool)] -> String -> Shader -> Transform -> GL.Color4 Float -> RIO env AnimatedSprite
createAnimatedSprite anims current shader transform color = do
    vao <- createSquareVAO

    animations <- Map.fromList <$> mapM (\(k, f, i, d, l) -> (k,)<$>createAnimation f i d l) anims

    let fixedSize = fixSize (texSize $ head $ animTextures $ head $ Map.elems $ animations) (size transform)

    return $ AnimatedSprite animations (Just current) shader vao (transform { size = fixedSize }) color

updateAnimation :: Float -> Animation -> Animation
updateAnimation deltaTime (Animation texts True dur currentTime)
    | currentTime + deltaTime > dur = (Animation texts True dur (currentTime + deltaTime - dur))
    | otherwise = (Animation texts True dur (currentTime + deltaTime))
updateAnimation deltaTime (Animation texts False dur currentTime)
    | currentTime + deltaTime > dur = (Animation texts False dur currentTime)
    | otherwise = (Animation texts False dur (currentTime + deltaTime))


getCurrentTexture :: Animation -> Texture
getCurrentTexture (Animation texts _ dur currentTime) = texts !! index
    where 
        len = realToFrac $ length texts
        frac = currentTime / dur
        index = floor $ frac * len

getCurrentAnimation :: AnimatedSprite -> Maybe Animation
getCurrentAnimation sprite = case asprCurrentAnimation sprite of
    Just str -> Map.lookup str (asprAnimations sprite)
    Nothing -> Nothing

updateAnimatedSprite :: Float -> AnimatedSprite -> AnimatedSprite 
updateAnimatedSprite _ aspr@(AnimatedSprite anims Nothing _ _ _ _) = aspr
updateAnimatedSprite deltaTime aspr@(AnimatedSprite anims (Just str) _ _ _ _) = aspr { asprAnimations = animations }
    where animations = Map.update (Just . updateAnimation deltaTime) str anims

renderAnimatedSprite :: AnimatedSprite -> RIO env ()
renderAnimatedSprite aspr = do

    case getCurrentTexture <$> getCurrentAnimation aspr of
        Nothing -> return () 
        Just texture -> do

            let shader = asprShader aspr

            useShader shader

            let transform = asprTransform aspr
            -- let fixedSize = fixSize (texSize texture) (size transform)
            model <- toGLMatrix $ getTransformMatrix $ transform  -- { size = fixedSize }
            

            uniform shader "model" $= model

            uniform shader "spriteColor" $= asprColor aspr

            GL.activeTexture $= GL.TextureUnit 0
            bindTexture texture

            GL.bindVertexArrayObject $= Just (asprVAO aspr)
            liftIO $ GL.drawArrays GL.Triangles 0 6
            GL.bindVertexArrayObject $= Nothing


fixSize :: GL.Vector2 Int -> GL.Vector3 Float -> GL.Vector3 Float
fixSize (GL.Vector2 w h) (GL.Vector3 x y z) 
    | abs x < 0.01 = (GL.Vector3 (realToFrac w * y / realToFrac h) y z)
    | abs y < 0.01 = (GL.Vector3 x (realToFrac h * x / realToFrac w) z)
    | otherwise = (GL.Vector3 x y z)



-- TODO: Theres a better way to do this (duplicated from Sprite, I guess every sprite is animated...)
updateASpritePos :: (GL.Vector3 Float -> GL.Vector3 Float) -> AnimatedSprite -> AnimatedSprite
updateASpritePos f sprite = sprite { asprTransform = trans { position = f pos }}
    where
        trans = asprTransform sprite
        pos = position trans

updateASpriteScale :: (GL.Vector3 Float -> GL.Vector3 Float) -> AnimatedSprite -> AnimatedSprite
updateASpriteScale f sprite = sprite { asprTransform = trans { size = f s }}
    where
        trans = asprTransform sprite
        s = size trans