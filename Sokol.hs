{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Sokol
    ( -- * Setup/shutdown
      setup
    , shutdown
    , commit
      -- * Pass actions
    , beginDefaultPass
    , endPass
      -- * Resources
    , Buffer(..)
    , Shader(..)
    , Pipeline(..)
    , makeVertexBuffer
    , makeShaderGLSL
    , makePipelineSimple
    , makePipelinePosColor
      -- * Drawing
    , applyPipeline
    , applyBindingsSimple
    , draw
      -- * Constants
    , vertexFormatFloat2
    , vertexFormatFloat3
    ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

-- Resource handles (sokol uses structs with uint32_t id field)
newtype Buffer = Buffer CUInt deriving (Eq, Show)
newtype Shader = Shader CUInt deriving (Eq, Show)
newtype Pipeline = Pipeline CUInt deriving (Eq, Show)

instance Storable Buffer where
    sizeOf _ = sizeOf (0 :: CUInt)
    alignment _ = alignment (0 :: CUInt)
    peek ptr = Buffer <$> peek (castPtr ptr)
    poke ptr (Buffer x) = poke (castPtr ptr) x

instance Storable Shader where
    sizeOf _ = sizeOf (0 :: CUInt)
    alignment _ = alignment (0 :: CUInt)
    peek ptr = Shader <$> peek (castPtr ptr)
    poke ptr (Shader x) = poke (castPtr ptr) x

instance Storable Pipeline where
    sizeOf _ = sizeOf (0 :: CUInt)
    alignment _ = alignment (0 :: CUInt)
    peek ptr = Pipeline <$> peek (castPtr ptr)
    poke ptr (Pipeline x) = poke (castPtr ptr) x

-- Constants for vertex formats (from sokol_gfx.h)
vertexFormatFloat2 :: CInt
vertexFormatFloat2 = 9

vertexFormatFloat3 :: CInt
vertexFormatFloat3 = 10

-- FFI imports to our C helpers
foreign import ccall "sokol_setup_simple"
    c_sokol_setup_simple :: CInt -> IO ()

foreign import capi "sokol/sokol_gfx.h sg_shutdown"
    c_sg_shutdown :: IO ()

foreign import capi "sokol/sokol_gfx.h sg_commit"
    c_sg_commit :: IO ()

foreign import ccall "sokol_begin_default_pass"
    c_sokol_begin_default_pass :: CInt -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import capi "sokol/sokol_gfx.h sg_end_pass"
    c_sg_end_pass :: IO ()

foreign import ccall "sokol_make_vertex_buffer"
    c_sokol_make_vertex_buffer :: Ptr () -> CSize -> IO CUInt

foreign import ccall "sokol_make_shader_glsl"
    c_sokol_make_shader_glsl :: CString -> CString -> IO CUInt

foreign import ccall "sokol_make_pipeline_simple"
    c_sokol_make_pipeline_simple :: CUInt -> CSize -> CInt -> CInt -> IO CUInt

foreign import ccall "sokol_make_pipeline_pos_color"
    c_sokol_make_pipeline_pos_color :: CUInt -> IO CUInt

foreign import ccall "sokol_apply_pipeline_wrapper"
    c_sokol_apply_pipeline_wrapper :: CUInt -> IO ()

foreign import ccall "sokol_apply_bindings_simple"
    c_sokol_apply_bindings_simple :: CUInt -> IO ()

foreign import capi "sokol/sokol_gfx.h sg_draw"
    c_sg_draw :: CInt -> CInt -> CInt -> IO ()

-- High-level API
setup :: Int -> IO ()
setup sampleCount = c_sokol_setup_simple (fromIntegral sampleCount)

shutdown :: IO ()
shutdown = c_sg_shutdown

commit :: IO ()
commit = c_sg_commit

beginDefaultPass :: Int -> Int -> Float -> Float -> Float -> Float -> IO ()
beginDefaultPass width height r g b a =
    c_sokol_begin_default_pass
        (fromIntegral width)
        (fromIntegral height)
        (realToFrac r)
        (realToFrac g)
        (realToFrac b)
        (realToFrac a)

endPass :: IO ()
endPass = c_sg_end_pass

makeVertexBuffer :: Ptr () -> Int -> IO Buffer
makeVertexBuffer dataPtr size = do
    bufId <- c_sokol_make_vertex_buffer dataPtr (fromIntegral size)
    return (Buffer bufId)

makeShaderGLSL :: String -> String -> IO Shader
makeShaderGLSL vsSrc fsSrc = do
    shdId <- withCString vsSrc $ \vsPtr ->
                withCString fsSrc $ \fsPtr ->
                    c_sokol_make_shader_glsl vsPtr fsPtr
    return (Shader shdId)

makePipelineSimple :: Shader -> Int -> Int -> Int -> IO Pipeline
makePipelineSimple (Shader shdId) stride attrIndex format = do
    pipId <- c_sokol_make_pipeline_simple
                shdId
                (fromIntegral stride)
                (fromIntegral attrIndex)
                (fromIntegral format)
    return (Pipeline pipId)

makePipelinePosColor :: Shader -> IO Pipeline
makePipelinePosColor (Shader shdId) = do
    pipId <- c_sokol_make_pipeline_pos_color shdId
    return (Pipeline pipId)

applyPipeline :: Pipeline -> IO ()
applyPipeline (Pipeline pipId) = c_sokol_apply_pipeline_wrapper pipId

applyBindingsSimple :: Buffer -> IO ()
applyBindingsSimple (Buffer bufId) = c_sokol_apply_bindings_simple bufId

draw :: Int -> Int -> Int -> IO ()
draw baseElement numElements numInstances =
    c_sg_draw
        (fromIntegral baseElement)
        (fromIntegral numElements)
        (fromIntegral numInstances)
