# Web Interface Developer Docs

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

   ```bash
   npm install
   ```

2. Start the development server:

   ```bash
   npm run dev
   ```

3. Build for production:

   ```bash
   npm run build
   ```

4. Preview the production build:

   ```bash
   npm run preview
   ```

## Rust WebAssembly Integration

To compile the Rust crate to WebAssembly and use it in the web interface:

1. Install wasm-pack if you haven't already:

   ```bash
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
   // In src/lib.rs
   use wasm_bindgen::prelude::*;

   #[wasm_bindgen]
   pub fn render_dfa(parsed: JsValue) -> JsValue {
       // Your DFA generation logic here
   }
   ```

4. Compile the Rust crate to WebAssembly:

   ```bash
   wasm-pack build --target web
   ```

5. Import the generated WebAssembly module in the web interface:

   ```typescript
   // In web/src/services/Bridge.ts
   import * as wasm from '../../../pkg/uncrustable';

   export async function initAutomataWasm() {
     await wasm.default();
     return wasm;
   }

   export async function renderDiagram(program: string): Promise<string> {
      if (!isInitialized) await initialize();

      try {
         // Call the Rust function directly
         console.log('rendering mermaid with render_mermaid...');
         return wasm.render_mermaid(program);
      } catch (error) {
         console.error('Error in renderDiagram:', error);
         throw error as string;
      }
   }
   ```

## Future Enhancements

- Better error handling (e.g. let the user know where program encountered an error)
- Improving mermaid render for text

## Project Structure

- `/src` - Main source code
  - `/components` - React components for the UI
  - `/services` - Bridge service for contacting with wasm
  - `/wasm` - WebAssembly bindings and integration
