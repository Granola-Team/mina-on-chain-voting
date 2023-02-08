import { CssBaseline, StyledEngineProvider, ThemeProvider } from '@mui/material';
import type { Decorator, StoryContext, StoryFn } from '@storybook/react';

import { DarkTheme, LightTheme } from 'components/themes';

/**
 * Storybook decorator that adds the Material UI theme provider to the story
 * component hierarchy.
 */
export const withMuiTheme: Decorator = (Story: StoryFn, context: StoryContext) => {
  const { theme: themeKey } = context.globals;
  const Theme = themeKey === 'dark' ? DarkTheme : LightTheme;

  return (
    <StyledEngineProvider injectFirst>
      <ThemeProvider theme={Theme}>
        <CssBaseline />
        <Story />
      </ThemeProvider>
    </StyledEngineProvider>
  );
};
