import * as wasm from '../wasm/RustCore';

/**
 * Sends the automata program to the Rust WASM module for parsing,
 * typechecking, and generating a DFA.
 * Returns the generated DFA as a string
 */
export const renderMermaid = (program: string): Promise<string> => {
  return new Promise(async (resolve, reject) => {
    try {
      // Make sure WASM is initialized
      await wasm.init();

      // Use the generate_mermaid_diagram function directly
      const mermaidDiagram = await wasm.generate_mermaid_diagram(program);

      resolve(mermaidDiagram);
    } catch (error) {
      reject(`Error processing automata program: ${error}`);
    }
  });
};