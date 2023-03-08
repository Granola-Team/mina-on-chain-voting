import { Fragment, PropsWithChildren } from 'react';

import App, { type AppContext, type AppProps } from 'next/app';
import Head from 'next/head';

import { CacheProvider } from '@emotion/react';
import { CssBaseline } from '@mui/material';

import { createEmotionCache } from 'common/mui';
import { getThemeType } from 'common/theme';

import { ThemeProvider } from 'components/provider';
import { type ThemeType, DefaultThemeType } from 'components/themes';

import { Provider as JotaiProvider } from 'jotai';
import { useHydrateAtoms } from 'jotai/react/utils';
import { queryClientAtom } from 'jotai-tanstack-query';
import { SnackbarProvider } from 'notistack';

import { QueryClient } from '@tanstack/query-core';
import { QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';

/**
 * Client-side cache styles, shared for the whole session of the user in the browser.
 */
const clientSideEmotionCache = createEmotionCache();

interface NextAppProps extends AppProps {
  renderedTheme: ThemeType;
  emotionCache?: typeof clientSideEmotionCache;
}

/**
 * Initializing single query client to be shared between each queries.
 */
const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: Infinity,
    },
  },
});

const HydrateAtoms = ({ children }: PropsWithChildren) => {
  useHydrateAtoms([[queryClientAtom, queryClient]]);
  return <Fragment>{children}</Fragment>;
};

export default function _App({
  Component,
  pageProps,
  emotionCache = clientSideEmotionCache,
  renderedTheme,
}: NextAppProps) {
  return (
    <>
      <Head>
        <meta name="viewport" content="width=device-width, initial-scale=1" />

        <meta name="theme-color-dark" content="#F3F6F9" media="(prefers-color-scheme: dark)" />
        <meta name="theme-color-light" content="#F3F6F9" media="(prefers-color-scheme: light)" />

        <title>Mina On-Chain Voting</title>
      </Head>

      <CacheProvider value={emotionCache}>
        <ThemeProvider value={renderedTheme}>
          <QueryClientProvider client={queryClient}>
            <JotaiProvider>
              <HydrateAtoms>
                <SnackbarProvider>
                  <CssBaseline />
                  <Component {...pageProps} />
                </SnackbarProvider>
              </HydrateAtoms>
            </JotaiProvider>
            <ReactQueryDevtools />
          </QueryClientProvider>
        </ThemeProvider>
      </CacheProvider>
    </>
  );
}

_App.getInitialProps = async (_ctx: AppContext) => {
  const appProps = await App.getInitialProps(_ctx);
  const { ctx } = _ctx;

  const cookie = ctx.req?.headers.cookie;

  return {
    ...appProps,
    renderedTheme: getThemeType(cookie ? cookie : DefaultThemeType),
  };
};
