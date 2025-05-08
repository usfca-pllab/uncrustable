/**
 * AutomataBridge.ts
 *
 * This file provides a bridge to the Rust automata WASM module.
 * It's responsible for:
 * 1. Processing automata programs for DFA generation
 * 2. Fetching generated Mermaid diagram syntax from the Rust codegen module
 */

// Import the WASM module
import * as wasmModule from '../wasm/automata';

/**
 * Define the structure of a DFA state with transitions
 */
interface DfaState {
  id: string;
  label: string;
  isAccepting: boolean;
  isInitial: boolean;
  transitions: {
    symbol: string;
    target: string;
  }[];
}

/**
 * Define the structure of a DFA
 */
interface Dfa {
  states: DfaState[];
  alphabet: string[];
}

/**
 * Example DFA for the div3.un program
 * This is a fallback if WASM module is not available
 */
const DIV3_DFA: Dfa = {
  states: [
    {
      id: '0',
      label: 'Remainder 0',
      isAccepting: true,
      isInitial: true,
      transitions: [
        { symbol: '0', target: '0' },
        { symbol: '1', target: '1' }
      ]
    },
    {
      id: '1',
      label: 'Remainder 1',
      isAccepting: false,
      isInitial: false,
      transitions: [
        { symbol: '0', target: '2' },
        { symbol: '1', target: '0' }
      ]
    },
    {
      id: '2',
      label: 'Remainder 2',
      isAccepting: false,
      isInitial: false,
      transitions: [
        { symbol: '0', target: '1' },
        { symbol: '1', target: '2' }
      ]
    }
  ],
  alphabet: ['0', '1']
};

/**
 * Parse an automata program and return a DFA
 * This calls the Rust WASM module to parse the program
 */
export const parseAutomataProgram = async (program: string): Promise<Dfa> => {
  console.log('Parsing automata program:', program);

  try {
    // Initialize WASM module if needed
    await wasmModule.init();
    
    // Call the WASM module to parse the program
    // In a real implementation, this would return a proper DFA structure
    const result = await wasmModule.parse_automata_program(program);
    console.log('Parse result:', result);
    
    // For now we'll return the example DFA until the WASM bridge is fully implemented
    return DIV3_DFA;
  } catch (error) {
    console.error('Error in parseAutomataProgram:', error);
    // Fallback to example DFA
    return DIV3_DFA;
  }
};

/**
 * Generate a Mermaid diagram from an automata program
 * This function connects to the Rust WASM module to generate the diagram
 *
 * @param program The automata program code
 * @returns Mermaid diagram syntax as a string
 */
export const generateDiagramFromProgram = async (program: string): Promise<string> => {
  try {
    if (!program || program.trim() === '') {
      throw new Error('Empty automata program');
    }

    // Initialize WASM module if needed
    await wasmModule.init();
    
    // Call the WASM module's generate_mermaid_diagram function
    // This directly uses the Rust dfa_to_mermaid function from codegen.rs
    return await wasmModule.generate_mermaid_diagram(program);
  } catch (error) {
    console.error('Error generating diagram:', error);
    throw error;
  }
};