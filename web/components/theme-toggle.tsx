'use client';

import { useTheme } from 'next-themes';

import { Button } from 'components/core/button';

import { MoonIcon, SunIcon } from '@radix-ui/react-icons';

export const ThemeToggle = () => {
  const { theme, setTheme } = useTheme();

  const switchTheme = () => {
    switch (theme) {
      case 'light':
        setTheme('dark');
        break;
      case 'dark':
        setTheme('light');
        break;
    }
  };

  return (
    <Button variant="ghost" size="icon" onClick={switchTheme}>
      <SunIcon className="h-[1.2rem] w-[1.2rem] rotate-0 scale-100 transition-all dark:-rotate-90 dark:scale-0" />
      <MoonIcon className="absolute h-[1.2rem] w-[1.2rem] rotate-90 scale-0 transition-all dark:rotate-0 dark:scale-100" />
      <span className="sr-only">{theme === 'light' ? 'Light mode' : 'Dark mode'}</span>
    </Button>
  );
};
