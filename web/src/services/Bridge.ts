/**
 * Bridge module for interacting with WebAssembly Rust functions.
 * Provides TypeScript wrappers around Rust-compiled WASM functions.
 */
import * as wasm from '../wasm/pkg/uncrustable';

/**
 * Tracks whether the WASM module has been initialized.
 * @private
 */
let isInitialized = false;

/**
 * Initializes the WASM module.
 * This must be called before any other function.
 *
 * @returns A promise that resolves when initialization is complete
 * @throws Error if initialization fails
 */
export async function initialize(): Promise<void> {
  if (isInitialized) return;

  try {
    // Initialize the WASM module
    await wasm.default();
    isInitialized = true;
    console.log('WASM module initialized successfully');
  } catch (error) {
    console.error('Failed to initialize WASM module:', error);
    throw error as string;
  }
}

/**
 * Generates a Mermaid diagram from an automata program.
 *
 * @param program - The automata program source code
 * @returns A promise that resolves to the Mermaid diagram as a string
 * @throws Error if diagram generation fails
 */
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
