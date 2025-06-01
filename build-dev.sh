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

# check node_modules directory exists
if [ ! -d "web/node_modules" ]; then
    echo "node_modules directory does not exist. Installing dependencies..."
    cd web && npm install
else
    echo "node_modules directory exists. Skipping dependency installation."
    cd web
fi

# Build the web app
echo "Building web app..."
npm run dev

echo "App is running..."
