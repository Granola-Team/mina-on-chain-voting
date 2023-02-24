import { render, screen } from 'common/test';

import { ResultsOverview } from './ResultsOverview';

describe('V1', () => {
  describe(ResultsOverview, () => {
    it('renders default state', () => {
      render(<ResultsOverview />);
      expect(screen.getByText('Results Overview')).toBeInTheDocument();

      expect(screen.getByText('FAQ')).toBeInTheDocument();
      expect(screen.getByText('FAQ').closest('a')).toHaveAttribute(
        'href',
        'https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959'
      );

      expect(screen.getByText('instructions')).toBeInTheDocument();
      expect(screen.getByText('instructions').closest('a')).toHaveAttribute(
        'href',
        'https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md'
      );

      expect(screen.getByText('feedback')).toBeInTheDocument();
      expect(screen.getByText('feedback').closest('a')).toHaveAttribute(
        'href',
        'https://docs.google.com/forms/d/e/1FAIpQLSeKoyUIVU3OrJ7hkakwHnOeWz9R8gRe-pUeduXeMyfFsmW6iQ/viewform?usp=sf_link'
      );
    });
  });
});
