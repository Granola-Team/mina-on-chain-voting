import { siteConfig } from 'common/config';
import { cleanup, MockNextNavigation, render, screen, TestVariant } from 'common/test';

import { NavigationMobile } from 'components/layout-nav-mobile';

jest.mock('next/navigation', () => MockNextNavigation);

describe(TestVariant.Component, () => {
  describe(NavigationMobile, () => {
    let user: ReturnType<typeof render>['user'];

    beforeEach(() => {
      const result = render(<NavigationMobile />);
      user = result.user;
    });

    afterEach(() => {
      cleanup();
    });

    it('renders toggle button', () => {
      const toggleButton = screen.getByRole('button', { name: /Toggle Menu/i });
      expect(toggleButton).toBeVisible();
    });

    it('renders links correctly', async () => {
      const toggleButton = screen.getByRole('button', { name: /Toggle Menu/i });
      await user.click(toggleButton);
      siteConfig.nav.forEach((item) => {
        const navLink = screen.getByRole('link', { name: item.title });
        expect(navLink).toHaveAttribute('href', item.href);
        expect(navLink).toBeVisible();
      });
    });

    it('opens and closes sidebar', async () => {
      const toggleButton = screen.getByRole('button', { name: /Toggle Menu/i });
      await user.click(toggleButton);

      const mobileLinks = screen.getByRole('link', { name: siteConfig.title });
      expect(mobileLinks).toBeVisible();

      const closeButton = screen.getByRole('button', { name: /Close/i });
      expect(closeButton).toBeVisible();

      await user.click(closeButton);

      expect(mobileLinks).not.toBeInTheDocument();
    });

    it('opens sidebar and navigates to link', async () => {
      const pushSpy = jest.spyOn(MockNextNavigation.router, 'push');
      const toggleButton = screen.getByRole('button', { name: /Toggle Menu/i });
      await user.click(toggleButton);

      const mobileLink = screen.getByRole('link', { name: siteConfig.title });
      expect(mobileLink).toBeVisible();

      await user.click(mobileLink);
      expect(pushSpy).toHaveBeenCalledWith('/');
    });
  });
});
