import { cleanup, render, screen, TestVariant } from 'common/test';

import { ThemeToggle } from 'components/theme-toggle';

describe(TestVariant.Component, () => {
  describe(ThemeToggle, () => {
    let user: ReturnType<typeof render>['user'];

    beforeEach(() => {
      const result = render(<ThemeToggle />);
      user = result.user;
    });

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      expect(screen.queryByText('Dark mode')).toBeInTheDocument();
      expect(screen.queryByText('Light mode')).not.toBeInTheDocument();
    });

    it('switches theme', async () => {
      const button = screen.getByRole('button');

      expect(button).toBeInTheDocument();

      await user.click(button);

      expect(screen.queryByText('Light mode')).toBeInTheDocument();
      expect(screen.queryByText('Dark mode')).not.toBeInTheDocument();

      await user.click(button);

      expect(screen.queryByText('Dark mode')).toBeInTheDocument();
      expect(screen.queryByText('Light mode')).not.toBeInTheDocument();
    });
  });
});
