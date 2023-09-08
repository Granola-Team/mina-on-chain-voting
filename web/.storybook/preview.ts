import { withMuiTheme, withNextRouter, withSnackbar } from 'common/storybook';

export const decorators = [withMuiTheme, withNextRouter, withSnackbar];

export const parameters = {
  actions: { argTypesRegex: '^on[A-Z].*' },
  controls: {
    hideNoControlsWarning: true,
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/,
    },
  },
  options: {
    storySort: {
      order: ['Atoms', 'Molecules', 'Organisms'],
    },
  },
};
