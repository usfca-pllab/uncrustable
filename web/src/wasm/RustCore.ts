import * as wasm from './pkg/uncrustable';

// Placeholder for initialization status
let initialized = false;

/**
 * Initialize the WASM module
 * This should be called before any other function
 */
export async function init() {
  if (initialized) return;

  try {
    // Initialize the WASM module
    await wasm.default();
    initialized = true;
    console.log('WASM module initialized successfully');
  } catch (error) {
    console.error('Failed to initialize WASM module:', error);
    throw error;
  }
}

/**
 * Parse an automata program and generate a DFA
 *
 * @param program The automata program source code
 * @returns The parsed program representation
 */
export async function parse_automata_program(program: string) {
  if (!initialized) await init();

  try {
    // Call the Rust function directly
    console.log('Calling Rust parse_automata_program with:', program);
    return wasm.parse_automata_program(program);
  } catch (error) {
    console.error('Error in parse_automata_program:', error);
    throw error;
  }
}

/**
 * Typecheck an automata program
 *
 * @param program The automata program source code
 * @returns A boolean indicating if the program typechecked successfully
 */
export async function typecheck_automata_program(program: string): Promise<boolean> {
  if (!initialized) await init();

  try {
    return wasm.typecheck_automata_program(program);
  } catch (error) {
    console.error('Error in typecheck_automata_program:', error);
    throw error;
  }
}

/**
 * Generate a DFA from an automata program
 *
 * @param program The automata program source code
 * @returns The generated DFA structure
 */
export async function generate_dfa(program: string) {
  if (!initialized) await init();

  try {
    return wasm.generate_dfa(program);
  } catch (error) {
    console.error('Error in generate_dfa:', error);
    throw error;
  }
}

/**
 * Generate a Mermaid diagram from an automata program
 *
 * @param program The automata program source code
 * @returns A Promise that resolves to the Mermaid diagram as a string
 */
export async function generate_mermaid_diagram(program: string): Promise<string> {
  if (!initialized) await init();

  try {
    // Call the Rust function directly
    console.log('Calling Rust generate_mermaid_diagram with:', program);
    return wasm.generate_mermaid_diagram(program);
  } catch (error) {
    console.error('Error in generate_mermaid_diagram:', error);
    throw error;
  }
}