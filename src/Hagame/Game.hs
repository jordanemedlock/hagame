module Hagame.Game where

import Hagame.Types
import Hagame.Render
import Hagame.Shader

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Lens
import Control.Monad (when)
import Data.Time
import Control.Concurrent (threadDelay)
import qualified Data.HashMap.Strict as HM

glfwErrorCallback err msg = do 
    putStrLn "GLFW Error occurred: "
    print err
    putStrLn $ "With message: " ++ msg

glDebugCallback msg = do
    putStr "GL Error Occurred: "
    print msg

runFixedGame :: GameOptions
             -> a
             -> (GameInput -> GameOptions -> a -> (GameOptions, a))
             -> (a -> [DrawRect])
             -> IO ()
runFixedGame options initialState handleInput render = do
    GLFW.setErrorCallback $ Just glfwErrorCallback

    initialized <- GLFW.init
    when (not initialized) $ glfwErrorCallback GLFW.Error'NotInitialized "Game Initialization failed"

    window <- initWindow options

    startTime <- getCurrentTime

    Just colorShader <- loadShader "resources/shaders/color.vert" "resources/shaders/color.frag" Nothing

    state <- mainLoop options window initialState startTime handleInput render colorShader HM.empty 

    -- TODO: clean up assets, once again not sure how to do this in the pure context

    GLFW.terminate

initWindow options = do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)

    let winSize = options^.(window.size)

    Just window <- GLFW.createWindow (winSize^.width.to floor) (winSize^.height.to floor) (options^.window.name) Nothing Nothing

    GLFW.makeContextCurrent $ Just window
            
    GL.debugOutput $= GL.Enabled
    GL.debugMessageCallback $= Just glDebugCallback

    (width, height) <- GLFW.getFramebufferSize window
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.depthFunc $= (Just GL.Lequal)

    -- TODO: not sure how to do initialization with the pure implementation

    return window


mainLoop options window state previousTime handleInput render colorShader renderContext = do
    GLFW.pollEvents

    let color = options^.backgroundColor

    GL.clearColor $= GL.Color4 (color^.r.to realToFrac) (color^.g.to realToFrac) (color^.b.to realToFrac) (color^.a.to realToFrac)
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]


    -- TODO: update, will need to think about timing for that threadDelay and laziness
    -- TODO: render
    let renderables = render state
    newRenderContext <- renderRects colorShader renderables renderContext

    GLFW.swapBuffers window
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose
        then return state
        else do
            
            thisTime <- getCurrentTime
            let deltaTime = diffUTCTime thisTime previousTime
            let dt = realToFrac deltaTime :: Double
            let goal = 1 / (options^.fps.to realToFrac)
            when (dt < goal) (threadDelay (floor $ 1000000 * (goal - dt)))
            mainLoop options window state thisTime handleInput render colorShader newRenderContext 