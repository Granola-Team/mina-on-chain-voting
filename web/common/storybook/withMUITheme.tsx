import React from 'react';

import { CssBaseline, StyledEngineProvider, ThemeProvider } from '@mui/material';
import type { Story } from '@storybook/react';

import { DefaultTheme } from 'components/themes';

/**
 * Storybook decorator that adds the Material UI theme provider to the story
 * component hierarchy.
 */
export const withMuiTheme = (Story: Story) => {
  return (
    <StyledEngineProvider injectFirst>
      <ThemeProvider theme={DefaultTheme}>
        <CssBaseline />
        <Story />
      </ThemeProvider>
    </StyledEngineProvider>
  );
};
