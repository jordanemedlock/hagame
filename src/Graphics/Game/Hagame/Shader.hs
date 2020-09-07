module Graphics.Game.Hagame.Shader (
    loadShader, useShader, clearShader, deleteShader, uniform, Shader
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString as BS
import System.FilePath.Posix (takeExtensions)

data Shader = Shader GL.Program

loadShader :: String -> String -> Maybe String -> IO (Maybe Shader)
loadShader vertexFile fragmentFile mGeometryFile = do
    putStrLn "Reading Shader Files"
    if ((takeExtensions vertexFile) /= ".vert" && (takeExtensions fragmentFile) /= ".frag")
        then return Nothing
        else do
            vertexSource <- BS.readFile vertexFile
            fragmentSource <- BS.readFile fragmentFile
            mGeometrySource <- mIO BS.readFile mGeometryFile

            compileShader vertexSource fragmentSource mGeometrySource

compileShader :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> IO (Maybe Shader)
compileShader vertexSource fragmentSource mGeometrySource = do
    putStrLn "Creating Shaders"
    vertex <- GL.createShader GL.VertexShader
    fragment <- GL.createShader GL.FragmentShader

    GL.shaderSourceBS vertex $= vertexSource
    GL.shaderSourceBS fragment $= fragmentSource

    putStrLn "Compiling Shaders"
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
            putStrLn "Attaching Shaders"
            program <- GL.createProgram

            GL.attachShader program vertex
            GL.attachShader program fragment
            mIO (GL.attachShader program) mGeometry

            GL.linkProgram program

            programStatus <- GL.get $ GL.linkStatus program

            if programStatus
                then do
                    GL.deleteObjectNames $ [vertex, fragment] ++ maybe [] (:[]) mGeometry

                    return $ Just $ Shader program
                else return Nothing

            
useShader :: Shader -> IO ()
useShader (Shader shader) = GL.currentProgram $= Just shader

clearShader :: IO ()
clearShader = GL.currentProgram $= Nothing

deleteShader :: Shader -> IO ()
deleteShader (Shader shader) = GL.deleteObjectName shader

uniform :: GL.Uniform a => Shader -> String -> GL.StateVar a
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
            



mIO :: (a -> IO b) -> Maybe a -> IO (Maybe b)
-- mIO f (Just x) = Just <$> f x
-- mIO f Nothing = return Nothing
mIO = mapM
