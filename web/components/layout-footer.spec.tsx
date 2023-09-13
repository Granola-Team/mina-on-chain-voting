import { siteConfig } from 'common/config';
import { cleanup, render, screen, TestVariant } from 'common/test';

import { Footer } from 'components/layout-footer';

describe(TestVariant.Component, () => {
  describe(Footer, () => {
    beforeEach(() => {
      render(<Footer />);
    });

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      const gLink = screen.getByRole('link', { name: /Granola/i });
      expect(gLink).toHaveAttribute('href', siteConfig.links.granola);
      expect(gLink).toBeVisible();
    });
  });
});
