import type { ReactElement, ReactNode } from 'react';

import { memoryRouter } from 'next-router-mock';
import { MemoryRouterProvider } from 'next-router-mock/dist/MemoryRouterProvider';
import { ThemeProvider } from 'next-themes';

import { render, RenderOptions } from '@testing-library/react';
import userEvent from '@testing-library/user-event';

// workaround for missing Next 13 support in next-router-mock
// PR: https://github.com/scottrippey/next-router-mock/pull/103
const MockNextNavigation = {
  router: memoryRouter,
  useRouter: () => memoryRouter,
  usePathname: () => memoryRouter.asPath,
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  useSearchParams: () => new URLSearchParams(memoryRouter.query as any),
};

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
    <MemoryRouterProvider {...memoryRouterProps}>
      <ThemeProvider attribute="class" defaultTheme="dark" enableSystem disableTransitionOnChange>
        {children}
      </ThemeProvider>
    </MemoryRouterProvider>
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
export { MockNextNavigation };
