import { createContext, FC, useContext, useState } from 'react';

import { ThemeProvider as MUIThemeProvider } from '@mui/material/styles';

import { escapeHTML } from 'common/helpers';
import { getDefaultTheme, getThemeValue, ThemeValue } from 'common/theme';

import { DarkTheme, LightTheme, ThemeType } from 'components/themes';

interface ThemeContextValue {
  theme: ThemeValue;
  setTheme: (key: ThemeType) => void;
}

export const ThemeContext = createContext<ThemeContextValue>({
  theme: getDefaultTheme(),
  // eslint-disable-next-line @typescript-eslint/no-empty-function
  setTheme: () => {},
});

export interface ThemeContextProviderProps {
  value: ThemeType;
  children: React.ReactNode | React.ReactNode[];
}

/**
 * Provides context for theme variables.
 */
export const ThemeProvider: FC<ThemeContextProviderProps> = (props) => {
  const [theme, _setTheme] = useState<ThemeValue>(getThemeValue(props.value));

  const setTheme = (key: ThemeType) => {
    _setTheme(key === 'dark' ? { key: 'dark', value: DarkTheme } : { key: 'light', value: LightTheme });
    document.cookie = `theme=${escapeHTML(key)}; SameSite=strict`;
  };

  const value = { theme, setTheme };

  return (
    <ThemeContext.Provider value={value}>
      <MUIThemeProvider theme={theme.value}>{props.children}</MUIThemeProvider>
    </ThemeContext.Provider>
  );
};

/**
 * Provides access to theme variables.
 */
export const useTheme = () => {
  return useContext(ThemeContext);
};
