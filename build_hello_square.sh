#!/usr/bin/env bash

set -e  # Exit on error

echo "=== Building Hello Square ==="

# Check if we're in nix-shell
if ! command -v ghc &> /dev/null; then
    echo "GHC not found. Running in nix-shell..."
    exec nix-shell --run "./build_hello_square.sh"
fi

# Project directory
PROJECT_DIR="hello/hello_square"
BUILD_DIR="$PROJECT_DIR/build"
mkdir -p "$BUILD_DIR"

# Compile shaders with sokol-shdc (validates and optimizes)
echo "Compiling shaders with sokol-shdc..."
sokol-shdc -i "$PROJECT_DIR/shaders/square.glsl" -o "$PROJECT_DIR/shaders/square_compiled" -l glsl410 -f bare

# Get Sokol include path from nix-shell environment
SOKOL_INCLUDE=$(echo $NIX_CFLAGS_COMPILE | grep -o '\-isystem [^ ]*sokol[^ ]*' | head -1 | cut -d' ' -f2)
echo "Sokol include path: $SOKOL_INCLUDE"

# Compile Sokol C sources (from root directory)
echo "Compiling sokol_impl.c..."
gcc -c sokol_impl.c -o "$BUILD_DIR/sokol_impl.o" \
    -I"$SOKOL_INCLUDE" \
    -fPIC

echo "Compiling sokol_helpers.c..."
gcc -c sokol_helpers.c -o "$BUILD_DIR/sokol_helpers.o" \
    -I"$SOKOL_INCLUDE" \
    -fPIC

# Get GLFW compile and link flags
GLFW_CFLAGS=$(pkg-config --cflags glfw3)
GLFW_LIBS=$(pkg-config --libs glfw3)

# Get proper libGL from package config or system
LIBGL_PATH=$(pkg-config --variable=libdir gl 2>/dev/null || echo "/run/opengl-driver/lib")

echo "GLFW flags: $GLFW_CFLAGS"
echo "GLFW libs: $GLFW_LIBS"
echo "GL path: $LIBGL_PATH"

# Build with GHC
echo "Compiling Haskell code..."
cd "$PROJECT_DIR"
ghc Main.hs \
    $GLFW_CFLAGS \
    $GLFW_LIBS \
    -I"$SOKOL_INCLUDE" \
    -i../.. \
    "build/sokol_impl.o" \
    "build/sokol_helpers.o" \
    -L"$LIBGL_PATH" -lGL \
    -outputdir "build" \
    -o hello_square

echo ""
echo "=== Build complete! ==="
echo "Run ./hello/hello_square/hello_square and use arrow keys to move the square"
