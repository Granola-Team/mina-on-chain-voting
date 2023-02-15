import { render, screen, waitFor } from 'common/test';

import { PageLayout } from './PageLayout';

describe('V1', () => {
  describe(PageLayout, () => {
    it('renders default state', () => {
      render(<PageLayout />);
      expect(screen.getByText('Mina Governance')).toBeInTheDocument();
      expect(screen.getByText('Github')).toBeInTheDocument();
      expect(screen.getByText('Made with ❤️ by Granola')).toBeInTheDocument();
    });

    it('changes theme on toggle', async () => {
      const { user } = render(<PageLayout />);

      expect(screen.getByTestId('Brightness7Icon')).toBeInTheDocument();

      const toggleButton = screen.getByLabelText('theme-toggle');
      expect(toggleButton).toBeInTheDocument();

      await user.click(toggleButton);

      await waitFor(() => {
        expect(screen.getByTestId('Brightness4Icon')).toBeInTheDocument();
      });
    });
  });
});
