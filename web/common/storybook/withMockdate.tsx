import type { StoryContext, StoryFn } from '@storybook/react';

import mockdate from 'mockdate';

/**
 * Storybook decorator to mock the system date.
 */
export const withMockdate = (Story: StoryFn, { parameters }: StoryContext) => {
  mockdate.reset();

  if (parameters.mockdate) {
    mockdate.set(parameters.mockdate);
  }

  return <Story />;
};
