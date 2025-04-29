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
  const [svgAvailable, setSvgAvailable] = useState<boolean>(false);
  const chartRef = useRef<string>('');
  const lastThemeRef = useRef<string>(theme);

  const downloadSvg = () => {
    if (!containerRef.current) return;

    // Find the SVG element inside the container
    const svgElement = containerRef.current.querySelector('svg');
    if (!svgElement) return;

    // Create a copy of the SVG to ensure we don't modify the displayed version
    const svgCopy = svgElement.cloneNode(true) as SVGElement;

    // Convert SVG to a data URL
    const svgData = new XMLSerializer().serializeToString(svgCopy);
    const svgBlob = new Blob([svgData], { type: 'image/svg+xml;charset=utf-8' });
    const svgUrl = URL.createObjectURL(svgBlob);

    // Create a download link and trigger it
    const downloadLink = document.createElement('a');
    downloadLink.href = svgUrl;
    downloadLink.download = 'automata-diagram.svg';
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);

    // Clean up the object URL
    URL.revokeObjectURL(svgUrl);
  };

  useEffect(() => {
    // Re-render chart if theme or chart changes
    if (chart === chartRef.current && theme === lastThemeRef.current) return;

    chartRef.current = chart;
    lastThemeRef.current = theme;
    setSvgAvailable(false);

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
        mermaid.initialize({
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

        // Check if SVG was successfully generated
        setSvgAvailable(!!containerRef.current.querySelector('svg'));
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

  return (
    <div className="w-full h-full relative">
      {renderError ? (
        <div className="p-4 border rounded theme-error-container">
          <p className="font-bold">Error rendering diagram:</p>
          <p>{renderError}</p>
          <p className="mt-2 text-sm">Check your automata code for errors.</p>
        </div>
      ) : (
        <>
          {svgAvailable && (
            <div className="absolute top-4 right-4 z-10">
              <button
                onClick={downloadSvg}
                className="flex items-center gap-1 px-3 py-2 text-sm rounded-md theme-button-primary"
                aria-label="Download diagram as SVG"
                title="Download diagram as SVG"
              >
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" className="w-4 h-4">
                  <path fillRule="evenodd" d="M12 2.25a.75.75 0 01.75.75v11.69l3.22-3.22a.75.75 0 111.06 1.06l-4.5 4.5a.75.75 0 01-1.06 0l-4.5-4.5a.75.75 0 111.06-1.06l3.22 3.22V3a.75.75 0 01.75-.75zm-9 13.5a.75.75 0 01.75.75v2.25a1.5 1.5 0 001.5 1.5h13.5a1.5 1.5 0 001.5-1.5V16.5a.75.75 0 011.5 0v2.25a3 3 0 01-3 3H5.25a3 3 0 01-3-3V16.5a.75.75 0 01.75-.75z" clipRule="evenodd" />
                </svg>
                SVG
              </button>
            </div>
          )}
          <div
            ref={containerRef}
            className="w-full h-full flex justify-center items-center"
          />
        </>
      )}
    </div>
  );
};

export default MermaidComponent;