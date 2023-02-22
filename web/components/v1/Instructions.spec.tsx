import { fireEvent, render, screen, waitFor } from 'common/test';

import { Instructions } from './Instructions';

describe('V1', () => {
  describe(Instructions, () => {
    it('renders default state', () => {
      render(<Instructions keyword="" />);
      expect(screen.getByText('How to participate in On-Chain Voting')).toBeInTheDocument();
      expect(screen.getByText('(FAQs)')).toBeInTheDocument();
    });
    it('an <a> tag with corresponding href exists', async () => {
      const { getByText } = render(<Instructions keyword="" />);

      expect(getByText('(FAQs)')).toBeInTheDocument();
      const clickButton = getByText('(FAQs)');
      expect(clickButton).toBeInTheDocument();
      fireEvent.click(clickButton);

      const link = await waitFor(() => getByText('(FAQs)').closest('a'));
      await expect(link).toHaveAttribute(
        'href',
        'https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959'
      );
    });
    it('an <a> tag with corresponding href exists', async () => {
      const { getByText } = render(<Instructions keyword="" />);

      expect(getByText('feedback')).toBeInTheDocument();
      const clickButton = getByText('feedback');
      expect(clickButton).toBeInTheDocument();
      fireEvent.click(clickButton);

      const link = await waitFor(() => getByText('feedback').closest('a'));
      await expect(link).toHaveAttribute(
        'href',
        'https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md'
      );
    });
  });
});
