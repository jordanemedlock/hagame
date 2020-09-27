{-# LANGUAGE RankNTypes #-}

module Graphics.Game.Hagame.Game (
      GameOptions(..), defaultGameOptions, initGameWindow, mainLoop, HasGameWindow(..)
    , DebugCallback, ErrorCallback
) where

import RIO
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)

type ErrorCallback = forall env. HasLogFunc env => Utf8Builder -> RIO env ()
type DebugCallback = forall env. HasLogFunc env => GL.DebugMessage -> RIO env ()
-- type InitializeCallback a = forall env. HasLogFunc env => Maybe a -> RIO env (Maybe a) 
-- type UpdateCallback a = forall env. Maybe a -> GLFW.Window -> Double -> RIO env (Maybe a)
-- type RenderCallback a = forall env. Maybe a -> RIO env ()
-- type DeleteCallback a = forall env. Maybe a -> RIO env ()

-- | Options used to initialize the game, the whole game should be here
data GameOptions = 
    GameOptions { errorCallback :: ErrorCallback -- ^ GLFW error callback
                , debugCallback :: DebugCallback -- ^ OpenGL debug callback
                , windowSize :: (Int, Int) -- ^ Initial window size
                , windowTitle :: String -- ^ Initial window title
                -- , gameState :: Maybe a -- ^ Game's global state
                -- , initializeGame :: InitializeCallback a -- ^ Callback used to initialize the game
                -- , updateGame :: UpdateCallback a -- ^ Called to update the state ever loop before render
                -- , renderGame :: RenderCallback a -- ^ Called every loop iteration to render the game
                -- , deleteAssets :: DeleteCallback a -- ^ Called after game stops to remove assets/state
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
                                    -- , gameState = Nothing
                                    -- , initializeGame = \s -> return s
                                    -- , updateGame = \s w dt -> return s
                                    -- , renderGame = \s -> return ()
                                    -- , deleteAssets = \s -> return ()
                                    }

-- | Entry point for ever game made with Hagame.  
-- Creates the game window, initializes the game, starts the main loop then handles when it closes.
-- runGame :: HasLogFunc env => GameOptions a -> RIO env ()
-- runGame gameOptions = do
--     env <- ask
--     if (not initialized) 
--         then errorCallback gameOptions "Game Initialization Failed"
--         else do

--             (mwindow, state) <- initGame gameOptions

--             case mwindow of
--                 Nothing -> errorCallback gameOptions "Game Initialization Failed"
--                 Just window -> do

--                     startTime <- liftIO getCurrentTime

--                     state <- mainLoop gameOptions window state startTime
        
--                     deleteAssets gameOptions state
        
--                     liftIO GLFW.terminate

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
mainLoop :: HasGameWindow env 
         => (Double -> RIO env ()) 
         -> RIO env ()
mainLoop updateFunc = loop =<< liftIO getCurrentTime
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

            let window = getGameWindow env

            shouldClose <- liftIO do
                GLFW.swapBuffers window
                GLFW.windowShouldClose window

            when (not shouldClose) $ loop thisTime

