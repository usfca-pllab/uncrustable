# Automata Web Interface

A modern web interface for visualizing automata programs as DFA diagrams. This application is built with React, TypeScript, Vite, and Monaco Editor.

## Tech Stack

- **Framework**: Vite + React + TypeScript
- **Code Editor**: Monaco Editor (the VS Code engine)
- **DFA Rendering**: Mermaid.js
- **Styling**: Tailwind CSS
- **Bundling**: Vite (handles both TypeScript and future Wasm integration)

## Features

- Syntax highlighting for the automata language (`.un` files)
- Interactive code editor with autocompletion and bracket matching
- DFA visualization using Mermaid diagrams
- Modern and responsive UI with Tailwind CSS

## Getting Started

1. Install dependencies:
   ```
   npm install
   ```

2. Start the development server:
   ```
   npm run dev
   ```

3. Build for production:
   ```
   npm run build
   ```

4. Preview the production build:
   ```
   npm run preview
   ```

## Rust WebAssembly Integration

To compile the Rust crate to WebAssembly and use it in the web interface:

1. Install wasm-pack if you haven't already:
   ```
   cargo install wasm-pack
   ```

2. Create a WebAssembly target for the Rust crate by adding the following to `Cargo.toml` in the project root:
   ```toml
   [lib]
   crate-type = ["cdylib", "rlib"]

   [dependencies]
   wasm-bindgen = "0.2.90"
   ```

3. Create WebAssembly bindings in the Rust crate:
   ```rust
   // In src/lib.rs or a new file src/wasm.rs
   use wasm_bindgen::prelude::*;
   
   #[wasm_bindgen]
   pub fn parse_program(code: &str) -> JsValue {
       // Your parsing logic here
   }
   
   #[wasm_bindgen]
   pub fn generate_dfa(parsed: JsValue) -> JsValue {
       // Your DFA generation logic here
   }
   ```

4. Compile the Rust crate to WebAssembly:
   ```
   wasm-pack build --target web
   ```

5. Import the generated WebAssembly module in the web interface:
   ```typescript
   // Update src/wasm/automata.ts to use the actual WASM module
   import * as wasm from '../../../pkg/automata_project';

   export async function initAutomataWasm() {
     await wasm.default();
     return wasm;
   }
   ```

## Future Enhancements

- Integration with Rust-compiled WebAssembly for automata parsing and DFA generation
- Custom themes for the code editor and diagrams
- Ability to save and load automata programs

## Project Structure

- `/src` - Main source code
  - `/components` - React components for the UI
  - `/services` - Services for Monaco editor configuration and automata processing
  - `/wasm` - WebAssembly bindings and integration
