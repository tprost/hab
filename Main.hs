module Main where

import qualified GLFW
import qualified Sokol as SG
import System.Exit (exitFailure)
import Control.Monad (unless, when)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (sizeOf)

-- Triangle vertices: position (x, y) and color (r, g, b)
vertices :: [Float]
vertices =
    [ -- x      y     r    g    b
       0.0,   0.5,  1.0, 0.0, 0.0  -- top (red)
    , -0.5,  -0.5,  0.0, 1.0, 0.0  -- bottom-left (green)
    ,  0.5,  -0.5,  0.0, 0.0, 1.0  -- bottom-right (blue)
    ]

-- Vertex shader source (GLSL 330)
vertexShaderSource :: String
vertexShaderSource = unlines
    [ "#version 330"
    , "layout(location=0) in vec2 position;"
    , "layout(location=1) in vec3 color0;"
    , "out vec3 color;"
    , "void main() {"
    , "    gl_Position = vec4(position, 0.0, 1.0);"
    , "    color = color0;"
    , "}"
    ]

-- Fragment shader source (GLSL 330)
fragmentShaderSource :: String
fragmentShaderSource = unlines
    [ "#version 330"
    , "in vec3 color;"
    , "out vec4 frag_color;"
    , "void main() {"
    , "    frag_color = vec4(color, 1.0);"
    , "}"
    ]

main :: IO ()
main = do
    -- Initialize GLFW
    initSuccess <- GLFW.init
    unless initSuccess $ do
        putStrLn "Failed to initialize GLFW"
        exitFailure

    -- Create a window
    maybeWindow <- GLFW.createWindow 640 480 "Sokol Triangle"
    case maybeWindow of
        Nothing -> do
            putStrLn "Failed to create window"
            GLFW.terminate
            exitFailure
        Just window -> do
            -- Make the OpenGL context current
            GLFW.makeContextCurrent (Just window)

            -- Setup Sokol
            SG.setup 1

            -- Create vertex buffer
            let vertexSize = length vertices * sizeOf (0.0 :: Float)
            vbuf <- withArray vertices $ \ptr ->
                SG.makeVertexBuffer (castPtr ptr) vertexSize

            -- Create shader
            shader <- SG.makeShaderGLSL vertexShaderSource fragmentShaderSource

            -- Create pipeline with position and color attributes
            pipeline <- SG.makePipelinePosColor shader

            putStrLn "Triangle setup complete! Press ESC or close window to exit."

            -- Main loop
            mainLoop window vbuf pipeline

            -- Clean up
            SG.shutdown
            GLFW.destroyWindow window
            GLFW.terminate

mainLoop :: GLFW.Window -> SG.Buffer -> SG.Pipeline -> IO ()
mainLoop window vbuf pipeline = do
    -- Check if we should close
    shouldClose <- GLFW.windowShouldClose window
    unless shouldClose $ do
        -- Get actual framebuffer size
        (fbWidth, fbHeight) <- GLFW.getFramebufferSize window

        -- Begin rendering
        SG.beginDefaultPass fbWidth fbHeight 0.1 0.1 0.1 1.0  -- Dark gray background

        -- Draw triangle
        SG.applyPipeline pipeline
        SG.applyBindingsSimple vbuf
        SG.draw 0 3 1  -- Draw 3 vertices, 1 instance

        -- End rendering
        SG.endPass
        SG.commit

        -- Swap buffers
        GLFW.swapBuffers window

        -- Poll for events
        GLFW.pollEvents

        -- Check for ESC key
        escPressed <- GLFW.getKey window GLFW.Key'Escape
        when (escPressed == GLFW.KeyState'Pressed) $
            GLFW.setWindowShouldClose window True

        -- Continue loop
        mainLoop window vbuf pipeline
