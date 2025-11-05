{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyDataDecls #-}

module GLFW
    ( -- * Initialization and termination
      init
    , terminate
      -- * Window handling
    , Window
    , createWindow
    , destroyWindow
    , windowShouldClose
    , setWindowShouldClose
    , swapBuffers
    , makeContextCurrent
      -- * Event handling
    , pollEvents
      -- * Input handling
    , Key(..)
    , KeyState(..)
    , toKeyCode
    , fromKeyAction
    , getKey
    , getFramebufferSize
    ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Prelude hiding (init)

-- Opaque types
data GLFWwindow
data GLFWmonitor

type Window = Ptr GLFWwindow

-- Constants
type KeyCode = CInt
type KeyAction = CInt

-- Key codes (just a few essential ones)
data Key
    = Key'Unknown
    | Key'Escape
    | Key'Space
    | Key'Right
    | Key'Left
    | Key'Down
    | Key'Up
    | Key'A | Key'B | Key'C | Key'D | Key'E | Key'F | Key'G | Key'H
    | Key'I | Key'J | Key'K | Key'L | Key'M | Key'N | Key'O | Key'P
    | Key'Q | Key'R | Key'S | Key'T | Key'U | Key'V | Key'W | Key'X
    | Key'Y | Key'Z
    deriving (Eq, Show)

toKeyCode :: Key -> KeyCode
toKeyCode Key'Unknown = -1
toKeyCode Key'Space   = 32
toKeyCode Key'Escape  = 256
toKeyCode Key'Right   = 262
toKeyCode Key'Left    = 263
toKeyCode Key'Down    = 264
toKeyCode Key'Up      = 265
toKeyCode Key'A = 65
toKeyCode Key'B = 66
toKeyCode Key'C = 67
toKeyCode Key'D = 68
toKeyCode Key'E = 69
toKeyCode Key'F = 70
toKeyCode Key'G = 71
toKeyCode Key'H = 72
toKeyCode Key'I = 73
toKeyCode Key'J = 74
toKeyCode Key'K = 75
toKeyCode Key'L = 76
toKeyCode Key'M = 77
toKeyCode Key'N = 78
toKeyCode Key'O = 79
toKeyCode Key'P = 80
toKeyCode Key'Q = 81
toKeyCode Key'R = 82
toKeyCode Key'S = 83
toKeyCode Key'T = 84
toKeyCode Key'U = 85
toKeyCode Key'V = 86
toKeyCode Key'W = 87
toKeyCode Key'X = 88
toKeyCode Key'Y = 89
toKeyCode Key'Z = 90

data KeyState
    = KeyState'Released
    | KeyState'Pressed
    | KeyState'Repeating
    deriving (Eq, Show)

fromKeyAction :: KeyAction -> KeyState
fromKeyAction 0 = KeyState'Released
fromKeyAction 1 = KeyState'Pressed
fromKeyAction 2 = KeyState'Repeating
fromKeyAction _ = KeyState'Released

-- FFI Imports
foreign import capi "GLFW/glfw3.h glfwInit"
    c_glfwInit :: IO CInt

foreign import capi "GLFW/glfw3.h glfwTerminate"
    c_glfwTerminate :: IO ()

foreign import capi "GLFW/glfw3.h glfwCreateWindow"
    c_glfwCreateWindow :: CInt -> CInt -> CString -> Ptr GLFWmonitor -> Window -> IO Window

foreign import capi "GLFW/glfw3.h glfwDestroyWindow"
    c_glfwDestroyWindow :: Window -> IO ()

foreign import capi "GLFW/glfw3.h glfwWindowShouldClose"
    c_glfwWindowShouldClose :: Window -> IO CInt

foreign import capi "GLFW/glfw3.h glfwSetWindowShouldClose"
    c_glfwSetWindowShouldClose :: Window -> CInt -> IO ()

foreign import capi "GLFW/glfw3.h glfwPollEvents"
    c_glfwPollEvents :: IO ()

foreign import capi "GLFW/glfw3.h glfwSwapBuffers"
    c_glfwSwapBuffers :: Window -> IO ()

foreign import capi "GLFW/glfw3.h glfwMakeContextCurrent"
    c_glfwMakeContextCurrent :: Window -> IO ()

foreign import capi "GLFW/glfw3.h glfwGetKey"
    c_glfwGetKey :: Window -> KeyCode -> IO KeyAction

foreign import capi "GLFW/glfw3.h glfwGetFramebufferSize"
    c_glfwGetFramebufferSize :: Window -> Ptr CInt -> Ptr CInt -> IO ()

-- High-level API
init :: IO Bool
init = do
    result <- c_glfwInit
    return (result == 1)

terminate :: IO ()
terminate = c_glfwTerminate

createWindow :: Int -> Int -> String -> IO (Maybe Window)
createWindow width height title = do
    win <- withCString title $ \cTitle ->
        c_glfwCreateWindow (fromIntegral width) (fromIntegral height)
                          cTitle nullPtr nullPtr
    if win == nullPtr
        then return Nothing
        else return (Just win)

destroyWindow :: Window -> IO ()
destroyWindow = c_glfwDestroyWindow

windowShouldClose :: Window -> IO Bool
windowShouldClose win = do
    result <- c_glfwWindowShouldClose win
    return (result /= 0)

setWindowShouldClose :: Window -> Bool -> IO ()
setWindowShouldClose win shouldClose =
    c_glfwSetWindowShouldClose win (if shouldClose then 1 else 0)

pollEvents :: IO ()
pollEvents = c_glfwPollEvents

swapBuffers :: Window -> IO ()
swapBuffers = c_glfwSwapBuffers

makeContextCurrent :: Maybe Window -> IO ()
makeContextCurrent Nothing = c_glfwMakeContextCurrent nullPtr
makeContextCurrent (Just win) = c_glfwMakeContextCurrent win

getKey :: Window -> Key -> IO KeyState
getKey win key = do
    action <- c_glfwGetKey win (toKeyCode key)
    return (fromKeyAction action)

getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize win =
    alloca $ \widthPtr ->
        alloca $ \heightPtr -> do
            c_glfwGetFramebufferSize win widthPtr heightPtr
            width <- peek widthPtr
            height <- peek heightPtr
            return (fromIntegral width, fromIntegral height)
