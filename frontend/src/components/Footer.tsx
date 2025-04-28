import React from 'react';
import { useTheme } from '../contexts/ThemeContext';

const Footer: React.FC = () => {
  const { theme } = useTheme();
  
  return (
    <footer className={`text-center text-sm shadow-sm ${
      theme === 'dark'
        ? 'bg-green-800 text-gray-100'
        : 'bg-green-600 text-white'
    }`}>
      <p className="py-3">University of San Francisco Programming Languages Lab &copy; {new Date().getFullYear()}</p>
    </footer>
  );
};

export default Footer;