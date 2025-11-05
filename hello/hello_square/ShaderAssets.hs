{-# LANGUAGE TemplateHaskell #-}

module ShaderAssets where

import Data.FileEmbed (embedStringFile)

vertexShader :: String
vertexShader = $(embedStringFile "shaders/square_compiled_square_glsl410_vertex.glsl")

fragmentShader :: String
fragmentShader = $(embedStringFile "shaders/square_compiled_square_glsl410_fragment.glsl")
