'use client';

import { PropsWithChildren } from 'react';

import { ThemeProvider } from 'next-themes';
import NextTopLoader from 'nextjs-toploader';

export const Providers = ({ children }: PropsWithChildren) => {
  return (
    <ThemeProvider attribute="class" defaultTheme="dark" enableSystem disableTransitionOnChange>
      <NextTopLoader color="#EA580C" showSpinner={false} height={2} />
      {children}
    </ThemeProvider>
  );
};
