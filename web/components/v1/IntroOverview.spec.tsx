import { render, screen } from 'common/test';

import { IntroOverview } from './IntroOverview';

describe('V1', () => {
  describe(IntroOverview, () => {
    it('renders default state', () => {
      render(<IntroOverview />);
      expect(screen.getByText('Mina Improvement Proposals (MIPs)')).toBeInTheDocument();

      expect(screen.getByText('FAQ')).toBeInTheDocument();
      expect(screen.getByText('FAQ').closest('a')).toHaveAttribute(
        'href',
        'https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959'
      );

      expect(screen.getByText('video')).toBeInTheDocument();
      expect(screen.getByText('video').closest('a')).toHaveAttribute('href', 'https://youtu.be/R_rG0LMnY1I');
    });
  });
});
