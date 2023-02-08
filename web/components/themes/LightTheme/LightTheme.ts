import { type ThemeOptions, createTheme } from '@mui/material/styles';
import { deepmerge } from '@mui/utils';

import { BaseTheme } from '../BaseTheme';

/**
 * The light theme.
 */
const _LightTheme: ThemeOptions = {
  palette: {
    mode: 'light',
  },
};

export const LightTheme = createTheme(deepmerge(BaseTheme, _LightTheme));
