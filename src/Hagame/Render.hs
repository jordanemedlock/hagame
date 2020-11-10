{-# LANGUAGE TemplateHaskell, TupleSections, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module Hagame.Render where

import Hagame.Types 
import Hagame.Shader 
import Hagame.Utils 
import qualified Data.HashMap.Strict as HM
import Control.Lens
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable


renderRect :: Shader -> DrawRect -> GL.VertexArrayObject -> IO ()
renderRect shader rect vao = do
    model <- toGLMatrix $ getTransformMatrix $ rect^.rectangle

    uniform shader "model" $= model
    uniform shader "color" $= rect^.color

    GL.bindVertexArrayObject $= Just vao
    GL.drawArrays GL.Triangles 0 6
    GL.bindVertexArrayObject $= Nothing


renderRects :: Shader -> [DrawRect] -> HM.HashMap String [GL.VertexArrayObject] -> IO (HM.HashMap String [GL.VertexArrayObject])
renderRects shader rects context = do
    useShader shader 
    -- TODO: need to fit this into the context probably
    -- TODO: also need to figure out what to do for more complex things
    -- TODO: I built it like this so that I wouldn't have to worry about that right now

    let rectMap = HM.toList $ foldr addRectToMap HM.empty rects

    newContext <- HM.fromList <$> mapM (\(k, rs) -> (k,) <$> renderRects' shader rs (HM.lookupDefault [] k context)) rectMap

    return newContext

renderRects' :: Shader -> [DrawRect] -> [GL.VertexArrayObject] -> IO [GL.VertexArrayObject]
renderRects' shader (r:rs) (v:vs) = do 
    renderRect shader r v 
    vs' <- renderRects' shader rs vs
    return $ v : vs'
renderRects' shader (r:rs) [] = do
    v <- createSquareVAO
    renderRect shader r v 
    vs' <- renderRects' shader rs []
    return $ v : vs'
renderRects' _ [] vs = return vs

addRectToMap :: DrawRect -> HM.HashMap String [DrawRect] -> HM.HashMap String [DrawRect]
addRectToMap r hm = HM.alter helper (r^.queueName) hm
    where
        helper Nothing = Just [r]
        helper (Just xs) = Just $ r : xs




