import type { Meta, StoryObj } from '@storybook/react';

import { ExampleSection, ExampleSectionProps } from './ExampleSection';

export default {
  title: 'Molecules/ExampleSection',
  component: ExampleSection,
} as Meta<typeof ExampleSection>;

export const Default: StoryObj<ExampleSectionProps> = { args: { default: true } };
