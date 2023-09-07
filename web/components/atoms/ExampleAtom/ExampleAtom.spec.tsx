import { render, screen } from '@testing-library/react';

import { ExampleAtom } from './ExampleAtom';

describe('Atoms', () => {
  it('renders the ExampleAtom', () => {
    render(<ExampleAtom />);
    expect(screen.getByText('ExampleAtom!')).toBeInTheDocument();
  });
});
