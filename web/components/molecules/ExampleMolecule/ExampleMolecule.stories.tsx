import type { Meta, StoryObj } from '@storybook/react';

import { ExampleMolecule, ExampleMoleculeProps } from './ExampleMolecule';

export default {
  title: 'Molecules/ExampleMolecule',
  component: ExampleMolecule,
} as Meta<typeof ExampleMolecule>;

export const Default: StoryObj<ExampleMoleculeProps> = { args: { default: true } };
