import { render, screen } from '@testing-library/react';

import { ExampleSection } from './ExampleSection';

describe('Molecules', () => {
  it('renders the ExampleSection', () => {
    render(<ExampleSection />);
    expect(screen.getByText('ExampleSection!')).toBeInTheDocument();
  });
});
