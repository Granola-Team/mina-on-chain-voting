import type { ReactElement, ReactNode } from 'react';

import { MemoryRouterProvider } from 'next-router-mock/dist/MemoryRouterProvider/next-12';

import { ThemeProvider } from 'components/provider';
import { DefaultThemeType } from 'components/themes';

import { SnackbarProvider } from 'notistack';

import { render, RenderOptions } from '@testing-library/react';
import userEvent from '@testing-library/user-event';

type MemoryRouterProviderOptions = typeof MemoryRouterProvider.defaultProps;

export type CustomRenderOptions = Omit<RenderOptions, 'wrapper'> & {
  memoryRouter?: MemoryRouterProviderOptions;
};

type GlobalRenderProviderProps = {
  children: ReactNode;
  memoryRouterProps?: MemoryRouterProviderOptions;
};

const GlobalRenderProvider = ({ children, memoryRouterProps }: GlobalRenderProviderProps) => {
  return (
    <ThemeProvider value={DefaultThemeType}>
      <MemoryRouterProvider {...memoryRouterProps}>
        <SnackbarProvider>{children}</SnackbarProvider>
      </MemoryRouterProvider>
    </ThemeProvider>
  );
};

const customRender = (ui: ReactElement, options?: CustomRenderOptions) => {
  const user = userEvent.setup();

  const renderResult = render(ui, {
    wrapper: (props) => <GlobalRenderProvider {...props} memoryRouterProps={options?.memoryRouter} />,
    ...options,
  });

  return {
    ...renderResult,
    user,
  };
};

export * from '@testing-library/react';
export { customRender as render };

export * from './msw';
export { rest } from 'msw';
