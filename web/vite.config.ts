import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import wasm from 'vite-plugin-wasm';
import { resolve } from 'path';

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [
    react(),
    wasm()
  ],
  base: './', // Set base URL for GitHub Pages deployment
  build: {
    outDir: '../dist', // Output to /docs folder for GitHub Pages
    emptyOutDir: true,
  },
});