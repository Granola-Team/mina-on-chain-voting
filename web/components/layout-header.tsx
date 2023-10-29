import Link from 'next/link';

import { siteConfig } from 'common/config';
import { cn } from 'common/utils';

import { buttonVariants } from 'components/core/button';
import { Icons } from 'components/core/icons';
import { NavigationDesktop } from 'components/layout-nav-desktop';
import { NavigationMobile } from 'components/layout-nav-mobile';
import { ThemeToggle } from 'components/theme-toggle';

export const Header = () => {
  return (
    <header className="supports-backdrop-blur:bg-background/60 sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur">
      <div className="container flex h-14 items-center">
        <NavigationDesktop />
        <NavigationMobile />
        <div className="flex flex-1 items-center justify-end space-x-4">
          <nav className="flex items-center space-x-1">
            <Link href={siteConfig.links.FAQ}>
              <div
                className={cn(
                  buttonVariants({
                    variant: 'ghost',
                  }),
                  'w-9 px-0'
                )}
              >
                <span className="hover:text-foreground/80">FAQ</span>
              </div>
            </Link>
            <Link href={siteConfig.links.github}>
              <div
                className={cn(
                  buttonVariants({
                    variant: 'ghost',
                  }),
                  'w-9 px-0'
                )}
              >
                <Icons.gitHub className="h-4 w-4" />
                <span className="sr-only">GitHub</span>
              </div>
            </Link>
            <ThemeToggle />
          </nav>
        </div>
      </div>
    </header>
  );
};
