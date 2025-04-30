import React from 'react';

const Footer: React.FC = () => {
  return (
    <footer className="text-center text-sm shadow-sm theme-header">
      <p className="py-1">University of San Francisco Programming Languages Lab &copy; {new Date().getFullYear()}</p>
    </footer>
  );
};

export default Footer;