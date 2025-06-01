import React from 'react';
import Editor from '@monaco-editor/react';
import { useTheme } from '../contexts/ThemeContext';

interface MermaidEditorProps {
  mermaidCode: string;
  onMermaidCodeChange: (value: string | undefined) => void;
}

const MermaidEditor: React.FC<MermaidEditorProps> = ({
  mermaidCode,
  onMermaidCodeChange,
}) => {
  const { theme } = useTheme();
  const monacoTheme = theme === 'dark' ? 'vs-dark' : 'vs-light';

  return (
    <Editor
      height="100%"
      defaultLanguage="markdown"
      value={mermaidCode}
      onChange={onMermaidCodeChange}
      theme={monacoTheme}
      options={{
        minimap: { enabled: false },
        fontSize: 14,
        scrollBeyondLastLine: false,
      }}
    />
  );
};

export default MermaidEditor;