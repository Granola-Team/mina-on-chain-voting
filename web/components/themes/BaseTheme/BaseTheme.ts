import type { ThemeOptions } from '@mui/material/styles';

import { Inter } from '@next/font/google';

export const Font = Inter({
  subsets: ['latin'],
  display: 'swap',
  fallback: ['Helvetica', 'Arial', 'sans-serif'],
});

/**
 * The default typography theme.
 */
const Typography: ThemeOptions = {
  typography: {
    fontFamily: Font.style.fontFamily,
  },
};

export const BaseTheme: ThemeOptions = {
  ...Typography,
};
