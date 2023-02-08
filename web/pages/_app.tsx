import App, { type AppContext, type AppProps } from 'next/app';
import Head from 'next/head';

import { CacheProvider } from '@emotion/react';
import { CssBaseline } from '@mui/material';

import { createEmotionCache } from 'common/mui';
import { getThemeType } from 'common/theme';

import { ThemeProvider } from 'components/provider';
import { type ThemeType, DefaultThemeType } from 'components/themes';

/**
 * Client-side cache styles, shared for the whole session of the user in the browser.
 */
const clientSideEmotionCache = createEmotionCache();

interface NextAppProps extends AppProps {
  renderedTheme: ThemeType;
  emotionCache?: typeof clientSideEmotionCache;
}

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

        <title>Mina Governance</title>
      </Head>

      <CacheProvider value={emotionCache}>
        <ThemeProvider value={renderedTheme}>
          <CssBaseline />
          <Component {...pageProps} />
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
