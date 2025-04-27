import { useState, useEffect } from 'react';
import Editor from '@monaco-editor/react';
import { createAutomataMonacoLanguage } from './components/Monaco-Editor';
import { generateDiagramFromProgram } from './services/AutomataBridge';
import MermaidComponent from './components/Mermaid';
import ThemeToggle from './components/ThemeToggle';
import { useTheme } from './contexts/ThemeContext';

// Sample code for the automata language
const DEFAULT_CODE = `// div3.un - Checks if a binary number is divisible by 3
alphabet { '0', '1' }

// Implement a DFA with three states tracking division by 3
on input {
  let remainder = 0;  // Start with remainder 0

  for c in input {
    if c == '0' {
      remainder = (2 * remainder) % 3;
    } else if c == '1' {
      remainder = (2 * remainder + 1) % 3;
    }
  }

  accept if remainder == 0;  // Accept if divisible by 3
}`;

function App() {
  const { theme } = useTheme();
  const [code, setCode] = useState(DEFAULT_CODE);
  const [mermaidCode, setMermaidCode] = useState<string>('');
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    // Register the automata language with Monaco
    createAutomataMonacoLanguage();
  }, []);

  const handleEditorChange = (value: string | undefined) => {
    if (value !== undefined) {
      setCode(value);
    }
  };

  const handleVisualize = async () => {
    setIsLoading(true);
    setError(null);

    try {
      const diagram = await generateDiagramFromProgram(code);
      setMermaidCode(diagram);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'An unknown error occurred');
    } finally {
      setIsLoading(false);
    }
  };

  // Dynamically choose theme colors based on current theme
  const themePrefix = theme === 'dark' ? 'github-dark' : 'github-light';
  // Monaco editor theme based on current theme
  const monacoTheme = theme === 'dark' ? 'vs-dark' : 'vs-light';

  return (
    <div className={`flex flex-col h-screen bg-${themePrefix}-bg`}>
      <header className={`flex justify-between items-center shadow-md ${
        theme === 'dark' 
          ? 'bg-green-800 text-gray-100'
          : 'bg-green-600 text-white'
      }`}>
        <div className="flex items-center px-4 py-3">
          <h1 className="text-2xl font-semibold">Uncrustables</h1>
        </div>
        <div className="flex items-center space-x-4 px-4 py-3">
          <a
            href="https://github.com/usfca-pllab/uncrustable"
            target="_blank"
            rel="noopener noreferrer"
            className="hover:underline font-medium text-white"
          >
            GitHub
          </a>
          <ThemeToggle />
        </div>
      </header>

      <div className="flex flex-1 gap-4 h-full p-4">
        <div className="w-1/2 flex flex-col">
          <div className={`bg-${themePrefix}-canvas-overlay p-2 rounded shadow flex-1 flex flex-col`}>
            <div className="flex justify-between mb-2">
              <h2 className={`text-lg font-semibold text-${themePrefix}-text`}>Code Editor</h2>
              <button
                onClick={handleVisualize}
                disabled={isLoading}
                className={`px-4 py-1 rounded hover:opacity-90 disabled:opacity-50 text-white ${
                  theme === 'dark'
                    ? 'bg-github-dark-accent'
                    : 'bg-github-light-accent'
                }`}
              >
                {isLoading ? 'Processing...' : 'Visualize'}
              </button>
            </div>

            <div className={`flex-1 rounded`}>
              <Editor
                height="100%"
                defaultLanguage="c"
                defaultValue={code}
                onChange={handleEditorChange}
                theme={monacoTheme}
                options={{
                  minimap: { enabled: false },
                  fontSize: 14,
                  scrollBeyondLastLine: false,
                }}
              />
            </div>
          </div>
        </div>

        <div className={`w-1/2 bg-${themePrefix}-canvas-overlay p-4 rounded shadow`}>
          <h2 className={`text-lg font-semibold mb-2 text-${themePrefix}-text`}>DFA Visualization</h2>

          {error ? (
            <div className={`text-${themePrefix}-danger p-4 border border-${themePrefix}-danger rounded bg-${themePrefix}-canvas-inset bg-opacity-50`}>
              {error}
            </div>
          ) : isLoading ? (
            <div className="flex justify-center items-center h-full">
              <p className={`text-${themePrefix}-fg-muted`}>Generating diagram...</p>
            </div>
          ) : mermaidCode ? (
            <div className={`rounded p-4 flex-1 h-full`}>
              <MermaidComponent chart={mermaidCode} />
            </div>
          ) : (
            <div className="flex justify-center items-center h-full">
              <p className={`text-${themePrefix}-fg-muted`}>Press "Visualize" to generate a diagram</p>
            </div>
          )}
        </div>
      </div>

      <footer className={`text-center text-sm shadow-sm ${
        theme === 'dark'
          ? 'bg-green-800 text-gray-100'
          : 'bg-green-600 text-white'
      }`}>
        <p className="py-3">University of San Francisco Programming Languages Lab &copy; {new Date().getFullYear()}</p>
      </footer>
    </div>
  );
}

export default App;