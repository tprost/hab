#!/usr/bin/env bash

set -e  # Exit on error

echo "=== Building GLFW + Sokol Haskell Project ==="

# Check if we're in nix-shell
if ! command -v ghc &> /dev/null; then
    echo "GHC not found. Running in nix-shell..."
    exec nix-shell --run "./build.sh"
fi

# Create build directory
BUILD_DIR="build"
mkdir -p "$BUILD_DIR"

# Get Sokol include path from nix-shell environment
SOKOL_INCLUDE=$(echo $NIX_CFLAGS_COMPILE | grep -o '\-isystem [^ ]*sokol[^ ]*' | head -1 | cut -d' ' -f2)
echo "Sokol include path: $SOKOL_INCLUDE"

# Compile Sokol C sources
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

echo "GLFW flags: $GLFW_CFLAGS"
echo "GLFW libs: $GLFW_LIBS"

# Build with GHC
echo "Compiling Haskell code..."
ghc Main.hs \
    $GLFW_CFLAGS \
    $GLFW_LIBS \
    -I"$SOKOL_INCLUDE" \
    "$BUILD_DIR/sokol_impl.o" \
    "$BUILD_DIR/sokol_helpers.o" \
    -lGL \
    -outputdir "$BUILD_DIR" \
    -o hab

echo ""
echo "=== Build complete! ==="
echo "Run ./hab to see the triangle"
