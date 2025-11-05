{-# LANGUAGE TemplateHaskell #-}

module ShaderAssets where

import Data.FileEmbed (embedStringFile)

vertexShader :: String
vertexShader = $(embedStringFile "shaders/triangle_compiled_triangle_glsl410_vertex.glsl")

fragmentShader :: String
fragmentShader = $(embedStringFile "shaders/triangle_compiled_triangle_glsl410_fragment.glsl")
