import React, { useState, useRef } from 'react';
import AutomataEditor from './AutomataEditor';
import MermaidEditor from './MermaidEditor';

interface EditorTabsProps {
  automataCode: string;
  mermaidCode: string;
  onAutomataCodeChange: (value: string | undefined) => void;
  onMermaidCodeChange: (value: string | undefined) => void;
  onVisualize: () => void;
  isLoading: boolean;
  onFileUpload?: (event: React.ChangeEvent<HTMLInputElement>) => void;
}

const EditorTabs: React.FC<EditorTabsProps> = ({
  automataCode,
  mermaidCode,
  onAutomataCodeChange,
  onMermaidCodeChange,
  onVisualize,
  isLoading,
  onFileUpload,
}) => {
  const [activeTab, setActiveTab] = useState<'automata' | 'mermaid'>('automata');
  const fileInputRef = useRef<HTMLInputElement>(null);

  const handleUploadClick = () => {
    fileInputRef.current?.click();
  };

  return (
    <div className="flex flex-col h-full">
      {/* Tab Navigation */}
      <div className="editor-tab-bar">
        <div className="flex">
          <div
            role="tab"
            aria-selected={activeTab === 'automata'}
            tabIndex={0}
            className={`editor-tab ${activeTab === 'automata' ? 'editor-tab-active' : 'editor-tab-inactive'}`}
            onClick={() => setActiveTab('automata')}
            onKeyDown={(e) => {
              if (e.key === 'Enter' || e.key === ' ') {
                setActiveTab('automata');
              }
            }}
          >
            Automata
          </div>

          <div
            role="tab"
            aria-selected={activeTab === 'mermaid'}
            tabIndex={0}
            className={`editor-tab ${activeTab === 'mermaid' ? 'editor-tab-active' : 'editor-tab-inactive'}`}
            onClick={() => setActiveTab('mermaid')}
            onKeyDown={(e) => {
              if (e.key === 'Enter' || e.key === ' ') {
                setActiveTab('mermaid');
              }
            }}
          >
            Mermaid
          </div>
        </div>

        <div className="flex">
          {activeTab === 'automata' && onFileUpload && (
            <>
              <button
                onClick={handleUploadClick}
                className="editor-button editor-button-secondary mr-2"
                title="Upload .un file"
              >
                <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="inline-block mr-1">
                  <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path>
                  <polyline points="17 8 12 3 7 8"></polyline>
                  <line x1="12" y1="3" x2="12" y2="15"></line>
                </svg>
                Upload
              </button>
              <input
                type="file"
                ref={fileInputRef}
                onChange={onFileUpload}
                accept=".un"
                className="hidden"
                aria-label="Upload .un file"
              />
            </>
          )}
          <button
            onClick={onVisualize}
            disabled={isLoading}
            className="editor-button editor-button-primary"
          >
            {isLoading ? 'Processing...' : 'Visualize'}
          </button>
        </div>
      </div>

      {/* Tab Content */}
      <div className="flex-1" role="tabpanel">
        {activeTab === 'automata' && (
          <AutomataEditor
            code={automataCode}
            onCodeChange={onAutomataCodeChange}
          />
        )}
        {activeTab === 'mermaid' && (
          <MermaidEditor
            mermaidCode={mermaidCode}
            onMermaidCodeChange={onMermaidCodeChange}
          />
        )}
      </div>
    </div>
  );
};

export default EditorTabs;