import { useEffect, useRef, useState } from 'react';
import mermaid from 'mermaid';
import { useTheme } from '../contexts/ThemeContext';

// Initialize mermaid with basic configuration
// Theme-specific config will be applied during rendering
mermaid.initialize({
  startOnLoad: false, // Prevent automatic rendering
  securityLevel: 'loose',
  logLevel: 'fatal', // Prevent noise
  er: {
    diagramPadding: 20
  },
  flowchart: {
    diagramPadding: 20,
    useMaxWidth: true,
  },
  stateDiagram2: {
    useMaxWidth: true,
    diagramPadding: 10,
  },
  sequence: {
    diagramMarginX: 50,
    diagramMarginY: 10,
    actorMargin: 50,
    width: 150,
    height: 65,
    boxMargin: 10,
    boxTextMargin: 5,
    noteMargin: 10,
    messageMargin: 35
  }
} as any);

interface MermaidProps {
  chart: string;
}

const MermaidComponent: React.FC<MermaidProps> = ({ chart }) => {
  const { theme } = useTheme();
  const containerRef = useRef<HTMLDivElement>(null);
  // Using the key as a stable reference instead of generating IDs every render
  const mermaidId = useRef(`mermaid-${Math.random().toString(36).substring(2, 9)}`);
  const [renderError, setRenderError] = useState<string | null>(null);
  const chartRef = useRef<string>('');
  const lastThemeRef = useRef<string>(theme);

  useEffect(() => {
    // Re-render chart if theme or chart changes
    if (chart === chartRef.current && theme === lastThemeRef.current) return;

    chartRef.current = chart;
    lastThemeRef.current = theme;

    const renderChart = async () => {
      if (!containerRef.current) return;
      setRenderError(null);

      try {
        // Clear the container completely before new render
        containerRef.current.innerHTML = '';

        // Validate the chart syntax before rendering
        try {
          await mermaid.parse(chart);
        } catch (parseError) {
          console.error('Mermaid parse error:', parseError);
          throw new Error(`Invalid diagram syntax: ${parseError instanceof Error ? parseError.message : String(parseError)}`);
        }

        // Create a container for this render with a consistent ID
        const element = document.createElement('div');
        element.id = mermaidId.current;
        element.className = 'mermaid';
        element.textContent = chart;
        containerRef.current.appendChild(element);

        // Reset any previous rendering state to prevent duplicates and apply theme
        await mermaid.initialize({
          startOnLoad: false,
          theme: theme === 'dark' ? 'dark' : 'default',
          securityLevel: 'loose',
          logLevel: 'fatal',
          flowchart: {
            useMaxWidth: true,
            diagramPadding: 20,
          }
        });

        // Render the new diagram
        await mermaid.run({
          nodes: [element],
          suppressErrors: false
        });
      } catch (error) {
        console.error('Error rendering Mermaid chart:', error);
        setRenderError(error instanceof Error ? error.message : 'Syntax error in diagram');

        if (containerRef.current) {
          containerRef.current.innerHTML = '';
        }
      }
    };

    renderChart();
  }, [chart, theme]);

  const themePrefix = theme === 'dark' ? 'github-dark' : 'github-light';

  return (
    <div className="w-full h-full">
      {renderError ? (
        <div className={`text-${themePrefix}-danger p-4 border border-${themePrefix}-danger rounded bg-${themePrefix}-canvas-inset`}>
          <p className="font-bold">Error rendering diagram:</p>
          <p>{renderError}</p>
          <p className="mt-2 text-sm">Check your automata code for errors.</p>
        </div>
      ) : (
        <div
          ref={containerRef}
          className="w-full h-full flex justify-center items-center"
        />
      )}
    </div>
  );
};

export default MermaidComponent;