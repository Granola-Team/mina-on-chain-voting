import { MemoryRouterProvider } from 'next-router-mock/MemoryRouterProvider/next-12';

import { action } from '@storybook/addon-actions';
import type { Story } from '@storybook/react';

/**
 * Storybook decorator to mock the Next.js router.
 */
export const withNextRouter = (Story: Story) => (
  <MemoryRouterProvider url="/" onPush={action('router.push')}>
    <Story />
  </MemoryRouterProvider>
);
