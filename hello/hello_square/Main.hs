{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import qualified GLFW
import qualified Sokol
import qualified ShaderAssets
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (castPtr)
import Foreign.C.Types
import Data.IORef

-- Square vertices: position (x,y) + color (r,g,b)
-- Two triangles making a square, centered at origin
squareVertices :: [Float]
squareVertices =
    -- Triangle 1
    [ -0.2, -0.2,  1.0, 0.5, 0.0  -- bottom-left (orange)
    ,  0.2, -0.2,  0.0, 1.0, 0.5  -- bottom-right (cyan)
    , -0.2,  0.2,  0.5, 0.0, 1.0  -- top-left (purple)
    -- Triangle 2
    , -0.2,  0.2,  0.5, 0.0, 1.0  -- top-left (purple)
    ,  0.2, -0.2,  0.0, 1.0, 0.5  -- bottom-right (cyan)
    ,  0.2,  0.2,  1.0, 1.0, 0.0  -- top-right (yellow)
    ]

main :: IO ()
main = do
    -- Initialize GLFW
    success <- GLFW.init
    if not success
        then putStrLn "Failed to initialize GLFW"
        else do
            -- Create window
            maybeWindow <- GLFW.createWindow 640 480 "Hello Square - Use Arrow Keys"
            case maybeWindow of
                Nothing -> do
                    putStrLn "Failed to create window"
                    GLFW.terminate
                Just window -> do
                    GLFW.makeContextCurrent (Just window)

                    -- Initialize Sokol
                    Sokol.setup 1

                    -- Create shader with uniform support
                    shader <- Sokol.makeShaderGLSLWithVSUniform ShaderAssets.vertexShader ShaderAssets.fragmentShader

                    -- Create pipeline
                    pipeline <- Sokol.makePipelinePosColor shader

                    -- Position state
                    posX <- newIORef (0.0 :: Float)
                    posY <- newIORef (0.0 :: Float)

                    -- Create vertex buffer once (fixed vertices at origin)
                    vertexBuffer <- withArray (map realToFrac squareVertices :: [CFloat]) $ \ptr ->
                        Sokol.makeVertexBuffer (castPtr ptr) (length squareVertices * 4)

                    -- Main loop
                    let loop = do
                            shouldClose <- GLFW.windowShouldClose window
                            if shouldClose
                                then return ()
                                else do
                                    -- Handle keyboard input
                                    x <- readIORef posX
                                    y <- readIORef posY

                                    -- Arrow keys
                                    leftPressed <- GLFW.getKey window GLFW.Key'Left
                                    rightPressed <- GLFW.getKey window GLFW.Key'Right
                                    upPressed <- GLFW.getKey window GLFW.Key'Up
                                    downPressed <- GLFW.getKey window GLFW.Key'Down

                                    let speed = 0.01
                                    let newX = x - (if leftPressed == GLFW.KeyState'Pressed then speed else 0)
                                                 + (if rightPressed == GLFW.KeyState'Pressed then speed else 0)
                                    let newY = y - (if downPressed == GLFW.KeyState'Pressed then speed else 0)
                                                 + (if upPressed == GLFW.KeyState'Pressed then speed else 0)

                                    writeIORef posX newX
                                    writeIORef posY newY

                                    -- Get framebuffer size
                                    (fbWidth, fbHeight) <- GLFW.getFramebufferSize window

                                    -- Render
                                    Sokol.beginDefaultPass fbWidth fbHeight 0.1 0.1 0.1 1.0
                                    Sokol.applyPipeline pipeline
                                    Sokol.applyBindingsSimple vertexBuffer
                                    Sokol.applyUniformsVec2 newX newY  -- Pass position as uniform
                                    Sokol.draw 0 6 1
                                    Sokol.endPass
                                    Sokol.commit

                                    GLFW.swapBuffers window
                                    GLFW.pollEvents

                                    loop

                    loop

                    -- Cleanup
                    Sokol.shutdown
                    GLFW.destroyWindow window
                    GLFW.terminate
