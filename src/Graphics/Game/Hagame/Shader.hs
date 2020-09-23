module Graphics.Game.Hagame.Shader (
    loadShader, useShader, clearShader, deleteShader, uniform, Shader
) where

import RIO
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
-- import qualified Data.ByteString as BS
import System.FilePath.Posix (takeExtensions)

-- | Wraps an OpenGL program 
data Shader = Shader GL.Program

-- | Loads a shader from its respective GLSL files
loadShader  :: HasLogFunc env
            => String -- ^ Vertex shader file location.  Must be *.vert
            -> String -- ^ Fragment shader file location.  Must be *.frag
            -> Maybe String -- ^ Optional Geometry file location.  Must be *.geom
            -> RIO env Shader
loadShader vertexFile fragmentFile mGeometryFile = do
    logInfo "Reading Shader Files"
    if ((takeExtensions vertexFile) /= ".vert" && (takeExtensions fragmentFile) /= ".frag")
        then throwString "Wrong File extension for shader"
        else do
            vertexSource <- readFileBinary vertexFile
            fragmentSource <- readFileBinary fragmentFile
            mGeometrySource <- mapM readFileBinary mGeometryFile

            compileShader vertexSource fragmentSource mGeometrySource

-- | Compiles shader from its strings
compileShader   :: HasLogFunc env
                => ByteString -- ^ Vertex Soure
                -> ByteString -- ^ Fragment Source
                -> Maybe ByteString -- ^ Optional Geometry Source
                -> RIO env Shader
compileShader vertexSource fragmentSource mGeometrySource = do
    (v, vs, f, fs, mg, gs) <- liftIO do
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

        return (vertex, vertexStatus, fragment, fragmentStatus, mGeometry, geometryStatus)

    if (not $ vs && fs && gs)
        then do
            vertexInfoLog <- liftIO $ GL.get $ GL.shaderInfoLog v
            fragmentInfoLog <- liftIO $ GL.get $ GL.shaderInfoLog f
            geometryInfoLog <- liftIO $ case mg of
                Just geometry -> GL.get $ GL.shaderInfoLog geometry
                Nothing -> return ""

            logInfo "Failed to compile shaders with message: "
            logInfo $ "Vertex Shader: " <> fromString vertexInfoLog
            logInfo $ "Fragment Shader: " <> fromString fragmentInfoLog
            logInfo $ "Geometry Shader: " <> fromString geometryInfoLog

            throwString "Failed to load shaders"
        else liftIO do
            program <- GL.createProgram

            GL.attachShader program v
            GL.attachShader program f
            mapM (GL.attachShader program) mg

            GL.linkProgram program

            programStatus <- GL.get $ GL.linkStatus program

            if programStatus
                then do
                    GL.deleteObjectNames $ [v, f] ++ maybe [] (:[]) mg

                    return $ Shader program
                else throwString "Failed to load program"

-- | Sets the OpenGL current shader
useShader :: MonadIO m => Shader -> m ()
useShader (Shader shader) = GL.currentProgram $= Just shader

-- | Clears the OpenGL current shader
clearShader :: MonadIO m => m ()
clearShader = GL.currentProgram $= Nothing

-- | Deletes the shader
deleteShader :: MonadIO m => Shader -> m ()
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
            