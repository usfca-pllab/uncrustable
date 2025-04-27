import * as monaco from 'monaco-editor';

/**
 * Register the automata language with Monaco Editor
 */
export function createAutomataMonacoLanguage() {
  // Register the automata language
  monaco.languages.register({ id: 'automata' });

  // Define the language configuration for automata
  monaco.languages.setLanguageConfiguration('automata', {
    comments: {
      lineComment: '//',
    },
    brackets: [
      ['{', '}'],
      ['[', ']'],
      ['(', ')'],
    ],
    autoClosingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: "'", close: "'", notIn: ['string', 'comment'] },
    ],
    surroundingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: "'", close: "'" },
    ],
  });

  // Define the tokens for syntax highlighting
  monaco.languages.setMonarchTokensProvider('automata', {
    // Set defaultToken to invalid to see what you do not tokenize yet
    defaultToken: 'invalid',

    keywords: [
      'alphabet', 'on', 'input', 'accept', 'if', 'else', 
      'fn', 'let', 'match', 'as', 'for', 'in'
    ],
    
    typeKeywords: [
      'int', 'sym'
    ],
    
    operators: [
      '=', '>', '<', '!', '~', '?', ':', '==', '<=', '>=', '!=',
      '&&', '||', '+', '-', '*', '/', '%', '&', '|', '^', '<<',
      '>>', '>>>',
    ],

    // Symbols without any fancy checks
    symbols: /[=><!~?:&|+\-*\/\^%]+/,

    // C# style strings
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    // The main tokenizer
    tokenizer: {
      root: [
        // Identifiers and keywords
        [/[a-z_$][\w$]*/, { 
          cases: { 
            '@keywords': 'keyword',
            '@typeKeywords': 'type',
            '@default': 'identifier' 
          } 
        }],

        // Whitespace
        { include: '@whitespace' },

        // Numbers
        [/\d+/, 'number'],

        // Delimiters and operators
        [/[{}()\[\]]/, '@brackets'],
        [/[<>](?!@symbols)/, '@brackets'],
        [/@symbols/, { cases: { '@operators': 'operator', '@default': '' } }],
        
        // Strings (character literals in automata)
        [/'[^\\']'/, 'string'],
        [/'\\[nt\\']'/, 'string'],
      ],

      whitespace: [
        [/[ \t\r\n]+/, 'white'],
        [/\/\/.*$/, 'comment'],
      ],
    },
  });
}