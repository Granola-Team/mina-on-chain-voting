import { render, screen } from '@testing-library/react';

import { ExampleMolecule } from './ExampleMolecule';

describe('Molecules', () => {
  it('renders the ExampleMolecule', () => {
    render(<ExampleMolecule />);
    expect(screen.getByText('ExampleMolecule!')).toBeInTheDocument();
  });
});
