import { fireEvent, render, screen, waitFor } from 'common/test';

import { ResultsOverview } from './ResultsOverview';

describe('V1', () => {
  describe(ResultsOverview, () => {
    it('renders default state', () => {
      render(<ResultsOverview />);
      expect(screen.getByText('Results Overview')).toBeInTheDocument();
      expect(screen.getByText('FAQ')).toBeInTheDocument();
    });
    it('an <a> tag with corresponding href exists', async () => {
      const { getByText } = render(<ResultsOverview />);

      expect(getByText('FAQ')).toBeInTheDocument();
      const clickButton = getByText('FAQ');
      expect(clickButton).toBeInTheDocument();
      fireEvent.click(clickButton);

      const link = await waitFor(() => getByText('FAQ').closest('a'));
      await expect(link).toHaveAttribute(
        'href',
        'https://forums.minaprotocol.com/t/on-chain-voting-frequently-asked-questions-faq/5959'
      );
    });
    it('an <a> tag with corresponding href exists', async () => {
      const { getByText } = render(<ResultsOverview />);

      expect(getByText('instructions')).toBeInTheDocument();
      const clickButton = getByText('instructions');
      expect(clickButton).toBeInTheDocument();
      fireEvent.click(clickButton);

      const link = await waitFor(() => getByText('instructions').closest('a'));
      await expect(link).toHaveAttribute(
        'href',
        'https://github.com/Granola-Team/blog/blob/main/voting-results-instructions.md'
      );
    });
    it('an <a> tag with corresponding href exists', async () => {
      const { getByText } = render(<ResultsOverview />);

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
