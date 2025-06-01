/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  darkMode: 'class', // Enable dark mode with class strategy
  theme: {
    extend: {
      colors: {
        'github-dark': {
          bg: '#0d1117',
          text: '#c9d1d9',
          border: '#30363d',
          link: '#58a6ff',
          header: '#161b22',
          accent: '#238636',
          danger: '#f85149',
          warning: '#f0883e',
          success: '#3fb950',
          canvas: {
            default: '#0d1117',
            overlay: '#161b22',
            inset: '#010409',
            subtle: '#161b22',
          },
          fg: {
            default: '#c9d1d9',
            muted: '#8b949e',
            subtle: '#6e7681',
          },
        },
        'github-light': {
          bg: '#ffffff',
          text: '#24292f',
          border: '#d0d7de',
          link: '#0969da',
          header: '#f6f8fa',
          accent: '#2da44e',
          danger: '#cf222e',
          warning: '#9a6700',
          success: '#218bff',
          canvas: {
            default: '#ffffff',
            overlay: '#f6f8fa',
            inset: '#f6f8fa',
            subtle: '#f6f8fa',
          },
          fg: {
            default: '#24292f',
            muted: '#57606a',
            subtle: '#6e7781',
          },
        },
      },
    },
  },
  plugins: [],
}