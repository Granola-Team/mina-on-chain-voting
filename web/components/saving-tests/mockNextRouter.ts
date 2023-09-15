import { useRouter as useRouterActual } from 'next/router';

export function useMockedRouter() {
  const useRouter = jest.fn();
  useRouter.mockReturnValue({
    route: '/',
    pathname: '/',
    query: {},
    asPath: '/',
    push: jest.fn(),
    replace: jest.fn(),
    reload: jest.fn(),
    back: jest.fn(),
    prefetch: jest.fn(),
  });
  return useRouter;
}

export const useRouter = useRouterActual;
