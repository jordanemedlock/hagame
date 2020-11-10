module Hagame.Shader (
    loadShader, useShader, clearShader, deleteShader, uniform, Shader
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString as BS
import System.FilePath.Posix (takeExtensions)

-- | Wraps an OpenGL program 
data Shader = Shader GL.Program

-- | Loads a shader from its respective GLSL files
loadShader  :: String -- ^ Vertex shader file location.  Must be *.vert
            -> String -- ^ Fragment shader file location.  Must be *.frag
            -> Maybe String -- ^ Optional Geometry file location.  Must be *.geom
            -> IO (Maybe Shader)
loadShader vertexFile fragmentFile mGeometryFile = do
    putStrLn "Reading Shader Files"
    if ((takeExtensions vertexFile) /= ".vert" && (takeExtensions fragmentFile) /= ".frag")
        then return Nothing
        else do
            vertexSource <- BS.readFile vertexFile
            fragmentSource <- BS.readFile fragmentFile
            mGeometrySource <- mapM BS.readFile mGeometryFile

            compileShader vertexSource fragmentSource mGeometrySource

-- | Compiles shader from its strings
compileShader   :: BS.ByteString -- ^ Vertex Soure
                -> BS.ByteString -- ^ Fragment Source
                -> Maybe BS.ByteString -- ^ Optional Geometry Source
                -> IO (Maybe Shader)
compileShader vertexSource fragmentSource mGeometrySource = do
    vertex <- GL.createShader GL.VertexShader
    fragment <- GL.createShader GL.FragmentShader

    GL.shaderSourceBS vertex $= vertexSource
    GL.shaderSourceBS fragment $= fragmentSource

    GL.compileShader vertex
    GL.compileShader fragment

    vertexStatus <- GL.get $ GL.compileStatus vertex
    fragmentStatus <- GL.get $ GL.compileStatus fragment

    (geometryStatus, mGeometry) <- case mGeometrySource of
        Just geometrySource -> do
            geometry <- GL.createShader GL.GeometryShader

            GL.shaderSourceBS geometry $= geometrySource

            GL.compileShader geometry

            geometryStatus <- GL.get $ GL.compileStatus geometry

            return (geometryStatus, Just geometry)
        Nothing -> return (True, Nothing)

    if (not $ vertexStatus && fragmentStatus && geometryStatus)
        then do
            vertexInfoLog <- GL.get $ GL.shaderInfoLog vertex
            fragmentInfoLog <- GL.get $ GL.shaderInfoLog fragment
            geometryInfoLog <- case mGeometry of
                Just geometry -> GL.get $ GL.shaderInfoLog geometry
                Nothing -> return ""

            putStrLn "Failed to compile shaders with message: "
            putStrLn $ "Vertex Shader: " ++ vertexInfoLog
            putStrLn $ "Fragment Shader: " ++ fragmentInfoLog
            putStrLn $ "Geometry Shader: " ++ geometryInfoLog

            return Nothing
        else do
            program <- GL.createProgram

            GL.attachShader program vertex
            GL.attachShader program fragment
            mapM (GL.attachShader program) mGeometry

            GL.linkProgram program

            programStatus <- GL.get $ GL.linkStatus program

            if programStatus
                then do
                    GL.deleteObjectNames $ [vertex, fragment] ++ maybe [] (:[]) mGeometry

                    return $ Just $ Shader program
                else return Nothing

-- | Sets the OpenGL current shader
useShader :: Shader -> IO ()
useShader (Shader shader) = GL.currentProgram $= Just shader

-- | Clears the OpenGL current shader
clearShader :: IO ()
clearShader = GL.currentProgram $= Nothing

-- | Deletes the shader
deleteShader :: Shader -> IO ()
deleteShader (Shader shader) = GL.deleteObjectName shader

-- | Sets/gets the uniform value for shader
uniform :: GL.Uniform a 
        => Shader -- ^ Shader that you want to access
        -> String -- ^ Uniform name you want to access
        -> GL.StateVar a -- ^ Setter/Getter
uniform (Shader shader) name = GL.makeStateVar getter setter
    where 
        getter :: GL.Uniform a => IO a
        getter = do
            useShader (Shader shader)
            location <- GL.get $ GL.uniformLocation shader name
            GL.get $ GL.uniform location
        setter :: GL.Uniform a => a -> IO ()
        setter val = do
            useShader (Shader shader)
            location <- GL.get $ GL.uniformLocation shader name
            GL.uniform location $= val
            