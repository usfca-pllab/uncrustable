import React from 'react';
import Editor from '@monaco-editor/react';
import { useTheme } from '../contexts/ThemeContext';

interface AutomataEditorProps {
  code: string;
  onCodeChange: (value: string | undefined) => void;
}

const AutomataEditor: React.FC<AutomataEditorProps> = ({
  code,
  onCodeChange,
}) => {
  const { theme } = useTheme();
  const monacoTheme = theme === 'dark' ? 'vs-dark' : 'vs-light';

  return (
    <Editor
      height="100%"
      language="c"
      defaultValue={code}
      value={code}
      onChange={onCodeChange}
      theme={monacoTheme}
      options={{
        minimap: { enabled: false },
        fontSize: 14,
        scrollBeyondLastLine: false,
      }}
    />
  );
};

export default AutomataEditor;