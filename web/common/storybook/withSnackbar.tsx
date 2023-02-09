import type { Story } from '@storybook/react';

import { SnackbarProvider } from 'notistack';

export const withSnackbar = (Story: Story) => (
  <SnackbarProvider>
    <Story />
  </SnackbarProvider>
);
