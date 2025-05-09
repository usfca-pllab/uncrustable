#!/bin/bash
set -e

# Install wasm-pack if not installed
if ! command -v wasm-pack &> /dev/null; then
    echo "Installing wasm-pack..."
    cargo install wasm-pack
fi

# Build the WASM package
echo "Building WASM package..."
wasm-pack build --target web --out-dir web/src/wasm/pkg

echo "WASM build complete. Output is in web/src/wasm/pkg/"
