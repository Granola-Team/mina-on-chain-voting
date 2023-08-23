import { type ClassValue, clsx } from 'clsx';
import { twMerge } from 'tailwind-merge';

export const cn = (...inputs: ClassValue[]) => {
  return twMerge(clsx(inputs));
};

export const isEmpty = (s: string | null) => s == null || s.trim().length === 0;

export const ThemePrimaryColor: Record<'dark' | 'light', string> = {
  dark: '24.6 95% 53.1%',
  light: '20.5 90.2% 48.2%',
};
