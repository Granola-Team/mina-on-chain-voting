import type { ThemeOptions } from '@mui/material/styles';

/**
 * The default typography theme.
 */
const Typography: ThemeOptions = {
  typography: {
    fontFamily: [
      'Inter var',
      'ui-sans-serif',
      'system-ui',
      '-apple-system',
      'BlinkMacSystemFont',
      'Segoe UI',
      'Helvetica Neue',
      'sans-serif',
    ].join(', '),
  },
};

export const BaseTheme: ThemeOptions = {
  ...Typography,
  components: {
    MuiContainer: {
      styleOverrides: {
        maxWidthXl: {
          '&.MuiContainer-maxWidthXl': {
            maxWidth: '110rem',
          },
        },
      },
    },
  },
};
