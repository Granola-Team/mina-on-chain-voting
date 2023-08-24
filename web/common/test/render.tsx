import type { ReactElement, ReactNode } from 'react';

import { MemoryRouterProvider } from 'next-router-mock/dist/MemoryRouterProvider';

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
  return <MemoryRouterProvider {...memoryRouterProps}>{children}</MemoryRouterProvider>;
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
