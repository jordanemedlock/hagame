module Graphics.Game.Hagame.Window (
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
type UpdateCallback a = Maybe a -> Float -> IO (Maybe a)
type RenderCallback a = Maybe a -> IO ()
type DeleteCallback a = Maybe a -> IO ()

data GameOptions a = 
    GameOptions { errorCallback :: GLFW.ErrorCallback
                , debugCallback :: DebugCallback
                , keyCallback :: GLFW.KeyCallback
                , windowSize :: (Int, Int)
                , windowTitle :: String
                , gameState :: Maybe a
                , initializeGame :: InitializeCallback a
                , updateGame :: UpdateCallback a
                , renderGame :: RenderCallback a
                , deleteAssets :: DeleteCallback a
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
                                    , keyCallback = \w k i s m -> return ()
                                    , windowSize = (640, 480)
                                    , windowTitle = "Hagame Game"
                                    , gameState = Nothing
                                    , initializeGame = \s -> return s
                                    , updateGame = \s dt -> return s
                                    , renderGame = \s -> return ()
                                    , deleteAssets = \s -> return ()
                                    }

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

            GLFW.setKeyCallback window $ Just (keyCallback gameOptions)

            (width, height) <- GLFW.getFramebufferSize window
            GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

            GL.blend $= GL.Enabled
            GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

            state <- return $ gameState gameOptions

            state <- initializeGame gameOptions state

            return (Just window, state)



mainLoop gameOptions window state previousTime = do
    GLFW.pollEvents

    GL.clearColor $= (GL.Color4 0 0 0 1)
    GL.clear [GL.ColorBuffer]

    thisTime <- getCurrentTime
    let deltaTime = realToFrac $ diffUTCTime thisTime previousTime

    state <- updateGame gameOptions state deltaTime

    renderGame gameOptions state

    GLFW.swapBuffers window
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose 
        then return state
        else mainLoop gameOptions window state thisTime

    



