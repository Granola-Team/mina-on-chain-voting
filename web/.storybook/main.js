const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin').TsconfigPathsPlugin;

module.exports = {
  stories: ['../components/**/*.stories.@(js|jsx|ts|tsx)'],
  addons: ['@storybook/addon-links', '@storybook/addon-essentials', '@storybook/addon-interactions'],
  framework: '@storybook/react',
  features: {
    emotionAlias: false,
    storyStoreV7: true,
  },
  core: {
    builder: 'webpack5',
  },
  typescript: {
    reactDocgen: false,
  },
  webpackFinal: async (config) => {
    config.resolve.plugins = [
      ...(config.resolve.plugins || []),
      new TsconfigPathsPlugin({
        extensions: config.resolve.extensions,
      }),
    ];

    config.resolve.fallback = {
      ...(config.resolve.fallback || {}),
      http: require.resolve('stream-http'),
      https: require.resolve('https-browserify'),
      os: require.resolve('os-browserify/browser'),
      path: require.resolve('path-browserify'),
      process: require.resolve('process/browser'),
      stream: require.resolve('stream-browserify'),
      timers: require.resolve('timers-browserify'),
      zlib: require.resolve('browserify-zlib'),
    };

    return config;
  },
};
