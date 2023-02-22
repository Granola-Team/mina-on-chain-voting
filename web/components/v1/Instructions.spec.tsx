// TODO: remove eslint-disable
/* eslint-disable playwright/missing-playwright-await */
import { render, screen } from 'common/test';

import { Instructions } from './Instructions';

describe('V1', () => {
  describe(Instructions, () => {
    it('renders default state', () => {
      render(<Instructions keyword="" />);
      expect(screen.getByText('How to participate in On-Chain Voting')).toBeInTheDocument();

      expect(screen.getByText('(FAQs)')).toBeInTheDocument();
      expect(screen.getByText('(FAQs)').closest('a')).toHaveAttribute(
        'href',
        'https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959'
      );

      expect(screen.getByText('feedback')).toBeInTheDocument();
      expect(screen.getByText('feedback').closest('a')).toHaveAttribute(
        'href',
        'https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md'
      );
    });
  });
});
