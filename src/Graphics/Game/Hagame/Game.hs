module Graphics.Game.Hagame.Game (
      GameOptions(..), runGame, defaultGameOptions
    , DebugCallback, InitializeCallback, UpdateCallback
    , RenderCallback, DeleteCallback
) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Time (getCurrentTime, diffUTCTime)


type DebugCallback = GL.DebugMessage -> IO ()
type InitializeCallback a = Maybe a -> IO (Maybe a) 
type UpdateCallback a = Maybe a -> GLFW.Window -> Float -> IO (Maybe a)
type RenderCallback a = Maybe a -> IO ()
type DeleteCallback a = Maybe a -> IO ()

-- | Options used to initialize the game, the whole game should be here
data GameOptions a = 
    GameOptions { errorCallback :: GLFW.ErrorCallback -- ^ GLFW error callback
                , debugCallback :: DebugCallback -- ^ OpenGL debug callback
                , windowSize :: (Int, Int) -- ^ Initial window size
                , windowTitle :: String -- ^ Initial window title
                , gameState :: Maybe a -- ^ Game's global state
                , initializeGame :: InitializeCallback a -- ^ Callback used to initialize the game
                , updateGame :: UpdateCallback a -- ^ Called to update the state ever loop before render
                , renderGame :: RenderCallback a -- ^ Called every loop iteration to render the game
                , deleteAssets :: DeleteCallback a -- ^ Called after game stops to remove assets/state
                }

defaultErrCallback err msg = do
    putStrLn "GLFW Error Occurred: "
    print err 
    putStrLn $ "with message: " ++ msg

defaultDebugCallback msg = do
    putStr "GL Error Occurred: "
    print msg

defaultGameOptions = GameOptions    { errorCallback = defaultErrCallback
                                    , debugCallback = defaultDebugCallback
                                    , windowSize = (640, 480)
                                    , windowTitle = "Hagame Game"
                                    , gameState = Nothing
                                    , initializeGame = \s -> return s
                                    , updateGame = \s w dt -> return s
                                    , renderGame = \s -> return ()
                                    , deleteAssets = \s -> return ()
                                    }

-- | Entry point for ever game made with Hagame.  
-- Creates the game window, initializes the game, starts the main loop then handles when it closes.
runGame :: GameOptions a -> IO ()
runGame gameOptions = do
    GLFW.setErrorCallback $ Just (errorCallback gameOptions)

    initialized <- GLFW.init
    if (not initialized) 
        then errorCallback gameOptions GLFW.Error'NotInitialized "Game Initialization Failed"
        else do

            (mwindow, state) <- initGame gameOptions

            case mwindow of
                Nothing -> errorCallback gameOptions GLFW.Error'NotInitialized "Game Initialization Failed"
                Just window -> do

                    startTime <- getCurrentTime

                    state <- mainLoop gameOptions window state startTime
        
                    deleteAssets gameOptions state
        
                    GLFW.terminate

-- | Initializes the game window OpenGL and GLFW environments, and runs the user initialization func.
initGame :: GameOptions a -> IO (Maybe GLFW.Window, Maybe a)
initGame gameOptions = do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)

    let (width, height) = windowSize gameOptions

    mwindow <- GLFW.createWindow width height (windowTitle gameOptions) Nothing Nothing

    GLFW.makeContextCurrent mwindow

    case mwindow of
        Nothing -> return (Nothing, Nothing)
        Just window -> do
            
            GL.debugOutput $= GL.Enabled
            GL.debugMessageCallback $= Just (debugCallback gameOptions)

            (width, height) <- GLFW.getFramebufferSize window
            GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

            GL.blend $= GL.Enabled
            GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
            GL.depthFunc $= (Just GL.Lequal)

            state <- return $ gameState gameOptions

            state <- initializeGame gameOptions state

            return (Just window, state)


-- | Recursive game loop
mainLoop gameOptions window state previousTime = do
    GLFW.pollEvents

    GL.clearColor $= (GL.Color4 0 0 0 1)
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    thisTime <- getCurrentTime
    let deltaTime = realToFrac $ diffUTCTime thisTime previousTime

    state <- updateGame gameOptions state window deltaTime

    renderGame gameOptions state

    GLFW.swapBuffers window
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose 
        then return state
        else mainLoop gameOptions window state thisTime

