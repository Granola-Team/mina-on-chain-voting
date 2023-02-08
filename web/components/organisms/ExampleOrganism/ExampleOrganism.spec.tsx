import { render, screen } from '@testing-library/react';

import { ExampleOrganism } from './ExampleOrganism';

describe('Organisms', () => {
  it('renders the ExampleOrganism', () => {
    render(<ExampleOrganism />);
    expect(screen.getByText('ExampleOrganism!')).toBeInTheDocument();
  });
});
