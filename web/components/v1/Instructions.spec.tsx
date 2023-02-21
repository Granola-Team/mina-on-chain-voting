import { render, screen } from 'common/test';

import { Instructions } from './Instructions';

describe('V1', () => {
  describe(Instructions, () => {
    it('renders default state', () => {
      render(<Instructions keyword={''} />);
      expect(screen.getByText('How to participate in On-Chain Voting')).toBeInTheDocument();
      expect(screen.getByText('(FAQs)')).toBeInTheDocument();
    });

    it('navigates to new FAQ page on link click', async () => {
        const { user } = render(<Instructions keyword={''} />);

        expect(screen.getByText('(FAQs)')).toBeInTheDocument();

        const clickButton = screen.getByText('(FAQs)');
        expect(clickButton).toBeInTheDocument();

        await user.click(clickButton);

        await expect(screen.getByText('(FAQs)').closest('a'))
        .toHaveAttribute('href', 'https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959');
    });

    it('navigates to new instructions page on link click', async () => {
        const { user } = render(<Instructions keyword={''} />);

        expect(screen.getByText('feedback')).toBeInTheDocument();

        const clickButton = screen.getByText('feedback');
        expect(clickButton).toBeInTheDocument();

        await user.click(clickButton);

        await expect(screen.getByText('feedback').closest('a'))
            .toHaveAttribute('href', 'https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md');
    });
  });
});
