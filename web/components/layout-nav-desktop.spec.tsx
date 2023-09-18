import { siteConfig } from 'common/config';
import { cleanup, render, screen, TestVariant } from 'common/test';

import { NavigationDesktop } from 'components/layout-nav-desktop';

describe(TestVariant.Component, () => {
  describe(NavigationDesktop, () => {
    beforeEach(() => {
      render(<NavigationDesktop />);
    });

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      const titleLink = screen.getByRole('link', { name: siteConfig.title });
      expect(titleLink).toHaveAttribute('href', '/');
      expect(titleLink).toBeVisible();
    });
  });
});
