import { siteConfig } from 'common/config';
import { cleanup, render, screen, TestVariant } from 'common/test';

import { Header } from 'components/layout-header';

describe(TestVariant.Component, () => {
  describe(Header, () => {
    beforeEach(() => {
      render(<Header />, {});
    });

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      const gitHubLink = screen.getByRole('link', { name: /Github/i });
      expect(gitHubLink).toHaveAttribute('href', siteConfig.links.github);
      expect(gitHubLink).toBeVisible();
    });
  });
});
