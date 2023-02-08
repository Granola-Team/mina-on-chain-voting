/* eslint-disable @typescript-eslint/no-explicit-any */
import Document, { Head, Html, Main, NextScript } from 'next/document';

import createEmotionServer from '@emotion/server/create-instance';

import { createEmotionCache } from 'common/mui';

import { Font } from 'components/themes/BaseTheme';

export default class _Document extends Document {
  render() {
    return (
      <Html lang="en" className={Font.className}>
        <Head>
          <link rel="shortcut icon" href="/static/favicon.ico" />
          <meta name="emotion-insertion-point" content="" />
          {(this.props as any).emotionStyleTags}
        </Head>
        <body>
          <Main />
          <NextScript />
        </body>
      </Html>
    );
  }
}

_Document.getInitialProps = async (ctx) => {
  const originalRenderPage = ctx.renderPage;

  // Might make sense to share the same Emotion cache between all the SSR requests to speed up performance.
  // However, it can have global side effects.
  const cache = createEmotionCache();
  const { extractCriticalToChunks } = createEmotionServer(cache);

  ctx.renderPage = () =>
    originalRenderPage({
      enhanceApp: (App: any) =>
        function EnhanceApp(props) {
          return <App emotionCache={cache} {...props} />;
        },
    });

  const initialProps = await Document.getInitialProps(ctx);
  // This is important. It prevents Emotion to render invalid HTML.
  // See https://github.com/mui/material-ui/issues/26561#issuecomment-855286153
  const emotionStyles = extractCriticalToChunks(initialProps.html);
  const emotionStyleTags = emotionStyles.styles.map((style) => (
    <style
      data-emotion={`${style.key} ${style.ids.join(' ')}`}
      key={style.key}
      // eslint-disable-next-line react/no-danger
      dangerouslySetInnerHTML={{ __html: style.css }}
    />
  ));

  return {
    ...initialProps,
    emotionStyleTags,
  };
};
