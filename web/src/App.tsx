import { useState } from 'react';
import { generateDiagramFromProgram } from './services/AutomataBridge';
import MermaidComponent from './components/Mermaid';
import { useTheme } from './contexts/ThemeContext';
import Header from './components/Header';
import Footer from './components/Footer';
import EditorTabs from './components/EditorTabs';

// Sample code for the automata language
const DEFAULT_CODE = `
// accept binary numbers that are divisible by 3

alphabet: { '0', '1' }

// an example helper function
fn char_to_bit(c: sym) -> int[2] = match c {
  '0' -> 0 as int[2]
  '1' -> 1 as int[2]
}

// calculate remainder mod 3 while reading each bit.
let rem: int[3];

on input bit {
  // later on, we might consider coercing the result of the function call so
  // that the "as int[3]" part is not necessary.  we need to do this for
  // number literals anyway.
  rem = 2 as int[3] * rem + char_to_bit(bit) as int[3];
}

accept if rem == 0 as int[3]
}`;

function App() {
  useTheme();
  const [code, setCode] = useState(DEFAULT_CODE);
  const [mermaidCode, setMermaidCode] = useState<string>('');
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleEditorChange = (value: string | undefined) => {
    if (value !== undefined) {
      setCode(value);
    }
  };

  const handleMermaidCodeChange = (value: string | undefined) => {
    if (value !== undefined) {
      setMermaidCode(value);
    }
  };

  const handleFileUpload = (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;

    // Check if it's a .un file
    if (!file.name.endsWith('.un')) {
      setError('Please upload a file with .un extension');
      return;
    }

    const reader = new FileReader();
    reader.onload = (e) => {
      const content = e.target?.result as string;
      if (content) {
        setCode(content);
        setError(null);
      }
    };
    reader.onerror = () => {
      setError('Error reading the file');
    };
    reader.readAsText(file);
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

  return (
    <div className="flex flex-col h-screen theme-bg">
      <Header />

      <div className="flex flex-1 gap-1 h-full p-1">
        <div className="w-1/2 flex flex-col gap-1 shadow theme-panel">
          <h2 className="font-medium px-2 py-1 theme-text">Editor</h2>
          <div className="flex-1">
            <EditorTabs
              automataCode={code}
              mermaidCode={mermaidCode}
              onAutomataCodeChange={handleEditorChange}
              onMermaidCodeChange={handleMermaidCodeChange}
              onVisualize={handleVisualize}
              isLoading={isLoading}
              onFileUpload={handleFileUpload}
            />
          </div>
        </div>

        <div className="w-1/2 p-1 shadow flex flex-col theme-panel">
          <div className="flex flex-col h-full">
            <div className="flex justify-between items-center mb-2">
              <h2 className="font-medium px-2 py-1 theme-text">Diagram</h2>
            </div>

            {error ? (
              <div className="p-4 border theme-error-container">
                {error}
              </div>
            ) : isLoading ? (
              <div className="flex justify-center items-center h-full">
                <p className="theme-text-muted">Generating diagram...</p>
              </div>
            ) : mermaidCode ? (
              <div className="flex-1 h-full overflow-auto">
                <div className="h-full">
                  <MermaidComponent chart={mermaidCode} />
                </div>
              </div>
            ) : (
              <div className="flex justify-center items-center h-full">
                <p className="theme-text-muted">Press "Visualize" to generate a diagram</p>
              </div>
            )}
          </div>
        </div>
      </div>

      <Footer />
    </div>
  );
}

export default App;