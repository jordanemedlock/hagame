{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Graphics.Game.Hagame.Game (
    --   GameOptions(..), defaultGameOptions
    -- , DebugCallback, InitializeCallback, UpdateCallback
    -- , RenderCallback, DeleteCallback
    GameException(..), initGameWindow, runGameLoop
) where

import RIO
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)


-- type DebugCallback = GL.DebugMessage -> IO ()
-- type InitializeCallback a = Maybe a -> IO (Maybe a) 
-- type UpdateFunction env = Double -> RIO env ()
-- type RenderFunction env = RIO env ()
-- type DeleteCallback a = Maybe a -> IO ()

-- | Options used to initialize the game, the whole game should be here
-- data GameOptions a = 
--     GameOptions { errorCallback :: GLFW.ErrorCallback -- ^ GLFW error callback
--                 , debugCallback :: DebugCallback -- ^ OpenGL debug callback
--                 , windowSize :: (Int, Int) -- ^ Initial window size
--                 , windowTitle :: String -- ^ Initial window title
--                 , gameState :: Maybe a -- ^ Game's global state
--                 , initializeGame :: InitializeCallback a -- ^ Callback used to initialize the game
--                 , updateGame :: UpdateCallback a -- ^ Called to update the state ever loop before render
--                 , renderGame :: RenderCallback a -- ^ Called every loop iteration to render the game
--                 , deleteAssets :: DeleteCallback a -- ^ Called after game stops to remove assets/state
--                 }

-- defaultErrCallback err msg = return ()
    -- putStrLn "GLFW Error Occurred: "
    -- print err 
    -- putStrLn $ "with message: " ++ msg

-- defaultDebugCallback msg = return ()
    -- putStr "GL Error Occurred: "
    -- print msg

-- defaultGameOptions = GameOptions    { errorCallback = defaultErrCallback
--                                     , debugCallback = defaultDebugCallback
--                                     , windowSize = (640, 480)
--                                     , windowTitle = "Hagame Game"
--                                     , gameState = Nothing
--                                     , initializeGame = \s -> return s
--                                     , updateGame = \s w dt -> return s
--                                     , renderGame = \s -> return ()
--                                     , deleteAssets = \s -> return ()
--                                     }

data GameException = 
    WindowNotCreated String
    | WindowNotInitialized String
    deriving (Show)

instance Exception GameException


-- | Entry point for ever game made with Hagame.  
-- Creates the game window, initializes the game, starts the main loop then handles when it closes.
-- runGame :: GameOptions a -> IO ()
-- runGame gameOptions = do
--     GLFW.setErrorCallback $ Just (errorCallback gameOptions)

--     initialized <- GLFW.init
--     if (not initialized) 
--         then errorCallback gameOptions GLFW.Error'NotInitialized "Game Initialization Failed"
--         else do

--             -- (mwindow, state) <- initGame gameOptions

--             case mwindow of
--                 Nothing -> errorCallback gameOptions GLFW.Error'NotInitialized "Game Initialization Failed"
--                 Just window -> do

--                     startTime <- getCurrentTime

--                     state <- mainLoop gameOptions window state startTime
        
--                     deleteAssets gameOptions state
        
--                     GLFW.terminate

-- class HasWindowTitle env where
--     windowTitleL :: Lens' env String

-- class HasWindowSize env where
--     windowSizeL :: Lens' env (Int, Int)

    -- windowWidthL :: Lens' env Int
    -- windowWidthL = windowSizeL._1

    -- windowHeightL :: Lens' env Int
    -- windowHeightL = windowSizeL._2

-- | Initializes the game window OpenGL and GLFW environments, and runs the user initialization func.
-- initGame :: GameOptions a -> IO (Maybe GLFW.Window, Maybe a)
initGameWindow :: HasLogFunc env => String -> (Int, Int) -> RIO env GLFW.Window
initGameWindow windowTitle (width, height) = do
    env <- ask

    window <- liftIO do
        GLFW.setErrorCallback $ Just (\err msg -> runRIO env $ logError $ fromString msg)

        initialized <- GLFW.init

        when (not initialized) do
            Just (_, msg) <- GLFW.getError
            throwIO (WindowNotInitialized msg)

        GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
        GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
        GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
        GLFW.windowHint (GLFW.WindowHint'Resizable False)

        mwindow <- GLFW.createWindow width height windowTitle Nothing Nothing

        GLFW.makeContextCurrent mwindow

        case mwindow of
            Nothing -> do
                Just (_, msg) <- GLFW.getError
                throwIO (WindowNotCreated msg)
            Just window -> return window
    
    liftIO do
        GL.debugOutput $= GL.Enabled
        GL.debugMessageCallback $= Just (\msg -> runRIO env (logInfo $ fromString $ show msg))

        (width, height) <- GLFW.getFramebufferSize window
        GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        GL.depthFunc $= (Just GL.Lequal)

        -- state <- return $ gameState gameOptions

        -- state <- initializeGame gameOptions state

        return window


runGameLoop :: GLFW.Window -> (Double -> RIO env ()) -> RIO env () -> RIO env ()
runGameLoop window updateFunc renderFunc = loop =<< liftIO getCurrentTime
    where loop prevTime = do
            thisTime <- liftIO do
                GLFW.pollEvents
                GL.clearColor $= (GL.Color4 0 0 0 1)
                GL.clear [GL.ColorBuffer, GL.DepthBuffer]

                getCurrentTime
            
            let deltaTime = diffUTCTime thisTime prevTime

            updateFunc $ realToFrac deltaTime

            renderFunc 

            shouldClose <- liftIO do
                GLFW.swapBuffers window

                GLFW.windowShouldClose window

            if shouldClose 
                then return ()
                else loop thisTime




-- | Recursive game loop
-- mainLoop gameOptions window state previousTime = do
--     GLFW.pollEvents

--     GL.clearColor $= (GL.Color4 0 0 0 1)
--     GL.clear [GL.ColorBuffer, GL.DepthBuffer]

--     thisTime <- getCurrentTime
--     let deltaTime = diffUTCTime thisTime previousTime
--     let dt = (realToFrac deltaTime :: Double)

--     state <- updateGame gameOptions state window dt

--     renderGame gameOptions state

--     GLFW.swapBuffers window
--     shouldClose <- GLFW.windowShouldClose window
--     if shouldClose 
--         then return state
--         else mainLoop gameOptions window state thisTime

