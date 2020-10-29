{-# LANGUAGE RankNTypes #-}

module Hagame.Window where


import RIO
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)

type ErrorCallback = forall env. HasLogFunc env => Utf8Builder -> RIO env ()
type DebugCallback = forall env. HasLogFunc env => GL.DebugMessage -> RIO env ()

-- | Options used to initialize the game, the whole game should be here
data GameOptions = 
    GameOptions { errorCallback :: ErrorCallback -- ^ GLFW error callback
                , debugCallback :: DebugCallback -- ^ OpenGL debug callback
                , windowSize :: (Int, Int) -- ^ Initial window size
                , windowTitle :: String -- ^ Initial window title
                }

class HasGameWindow e where
    getGameWindow :: e -> GLFW.Window


defaultErrCallback :: ErrorCallback
defaultErrCallback msg = do
    logInfo "GLFW Error Occurred: "
    -- logInfo $ fromString err 
    logInfo $ "with message: " <> msg

defaultDebugCallback :: DebugCallback
defaultDebugCallback msg = do
    logInfo "GL Error Occurred: "
    logInfo $ fromString $ show msg

defaultGameOptions = GameOptions    { errorCallback = defaultErrCallback
                                    , debugCallback = defaultDebugCallback
                                    , windowSize = (640, 480)
                                    , windowTitle = "Hagame Game"
                                    }

-- | Initializes the game window OpenGL and GLFW environments, and runs the user initialization func.
initGameWindow :: HasLogFunc env => GameOptions -> RIO env (GLFW.Window)
initGameWindow gameOptions = do
    env <- ask

    liftIO $ GLFW.setErrorCallback $ Just (\err msg -> runRIO env $ errorCallback gameOptions (fromString msg))

    initialized <- liftIO GLFW.init

    when (not initialized) do
        errorCallback gameOptions "Game Initialization Failed"
        throwString "Game Initialization Failed"


    liftIO do
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
        GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
        GLFW.windowHint (GLFW.WindowHint'Resizable False)

    let (width, height) = windowSize gameOptions

    mwindow <- liftIO $ GLFW.createWindow width height (windowTitle gameOptions) Nothing Nothing

    liftIO $ GLFW.makeContextCurrent mwindow


    when (isNothing mwindow) do
        throwString "Window failed to initialize"
    

    let (Just window) = mwindow

    GL.debugOutput $= GL.Enabled
    GL.debugMessageCallback $= Just (\msg -> runRIO env $ debugCallback gameOptions msg)

    (width, height) <- liftIO $ GLFW.getFramebufferSize window
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.depthFunc $= (Just GL.Lequal)

    return window


-- | Recursive game loop
mainLoop    :: GLFW.Window 
            -> (Double -> RIO env ()) 
            -> RIO env ()
mainLoop window updateFunc = loop =<< liftIO getCurrentTime
    where 
        loop previousTime = do
            env <- ask 

            liftIO GLFW.pollEvents

            GL.clearColor $= (GL.Color4 0 0 0 1)
            liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

            thisTime <- liftIO getCurrentTime

            let deltaTime = diffUTCTime thisTime previousTime
            let dt = (realToFrac deltaTime :: Double)

            updateFunc dt

            shouldClose <- liftIO do
                GLFW.swapBuffers window
                GLFW.windowShouldClose window

            when (not shouldClose) $ loop thisTime

