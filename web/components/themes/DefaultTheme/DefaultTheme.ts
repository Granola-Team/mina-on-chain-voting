import type { Theme } from '@mui/material/styles';

import { DarkTheme } from '../DarkTheme';

export type ThemeType = 'light' | 'dark';

export const DefaultTheme: Theme = DarkTheme;
export const DefaultThemeType: ThemeType = 'dark';
