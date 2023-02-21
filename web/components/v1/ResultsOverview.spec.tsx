import { render, screen } from 'common/test';

import { ResultsOverview } from './ResultsOverview';

describe('V1', () => {
  describe(ResultsOverview, () => {
    it('renders default state', () => {
      render(<ResultsOverview />);
      expect(screen.getByText('Results Overview')).toBeInTheDocument();
      expect(screen.getByText('FAQ')).toBeInTheDocument();
    });

    it('navigates to new FAQ page on link click', async () => {
        const { user } = render(<ResultsOverview />);

        expect(screen.getByText('FAQ')).toBeInTheDocument();

        const clickButton = screen.getByText('FAQ');
        expect(clickButton).toBeInTheDocument();

        await user.click(clickButton);

        await expect(screen.getByText('FAQ').closest('a'))
        .toHaveAttribute('href', 'https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959');
    });

    it('navigates to new instructions page on link click', async () => {
        const { user } = render(<ResultsOverview />);

        expect(screen.getByText('instructions')).toBeInTheDocument();

        const clickButton = screen.getByText('instructions');
        expect(clickButton).toBeInTheDocument();

        await user.click(clickButton);

        await expect(screen.getByText('instructions').closest('a'))
            .toHaveAttribute('href', 'https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md');
    });

    it('navigates to instructions page on click for feedback link', async () => {
        const { user } = render(<ResultsOverview />);

        expect(screen.getByText('feedback')).toBeInTheDocument();

        const clickButton = screen.getByText('feedback');
        expect(clickButton).toBeInTheDocument();

        await user.click(clickButton);

        await expect(screen.getByText('feedback').closest('a'))
            .toHaveAttribute('href', 'https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md');
    });
  });
});
