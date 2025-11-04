{-# LANGUAGE TemplateHaskell #-}

module ShaderAssets where

import Data.FileEmbed (embedStringFile)

vertexShader :: String
vertexShader = $(embedStringFile "shaders/triangle.vert")

fragmentShader :: String
fragmentShader = $(embedStringFile "shaders/triangle.frag")
