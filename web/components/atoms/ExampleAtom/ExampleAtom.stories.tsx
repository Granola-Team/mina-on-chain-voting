import type { Meta, StoryObj } from '@storybook/react';

import { ExampleAtom, ExampleAtomProps } from './ExampleAtom';

export default {
  title: 'Atoms/ExampleAtom',
  component: ExampleAtom,
} as Meta<typeof ExampleAtom>;

export const Default: StoryObj<ExampleAtomProps> = { args: { default: true } };
