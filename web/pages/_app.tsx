import { Fragment, PropsWithChildren } from 'react';

import type { AppProps } from 'next/app';
import Head from 'next/head';

import { Provider as JotaiProvider } from 'jotai';
import { useHydrateAtoms } from 'jotai/react/utils';
import { queryClientAtom } from 'jotai-tanstack-query';

import { QueryClient } from '@tanstack/query-core';
import { QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';

import 'styles/globals.css';

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

export default function App({ Component, pageProps }: AppProps) {
  return (
    <>
      <Head>
        <meta name="viewport" content="width=device-width, initial-scale=1" />

        <meta name="theme-color-dark" content="#F3F6F9" media="(prefers-color-scheme: dark)" />
        <meta name="theme-color-light" content="#F3F6F9" media="(prefers-color-scheme: light)" />

        <title>Mina On-Chain Voting</title>
      </Head>

      <QueryClientProvider client={queryClient}>
        <JotaiProvider>
          <HydrateAtoms>
            <Component {...pageProps} />
          </HydrateAtoms>
        </JotaiProvider>
        <ReactQueryDevtools />
      </QueryClientProvider>
    </>
  );
}
