'use client';

import * as React from 'react';

import Link from 'next/link';
import { usePathname } from 'next/navigation';

import { siteConfig } from 'common/config';
import { cn } from 'common/utils';

export const NavigationDesktop = () => {
  const pathname = usePathname();

  return (
    <div className="mr-4 hidden md:flex">
      <Link href="/" className="mr-6 flex items-center space-x-2">
        <span className="hidden font-bold sm:inline-block">{siteConfig.title}</span>
      </Link>
      <nav className="flex items-center space-x-6 text-sm font-medium">
        {siteConfig.nav.map((item) =>
          item.href.includes('https') || item.href.includes('http') ? (
            <a
              key={item.href}
              href={item.href}
              rel="noreferrer"
              target="_blank"
              className={cn(
                'transition-colors hover:text-foreground/80',
                pathname === item.href ? 'text-foreground' : 'text-foreground/60'
              )}
            >
              {item.title}
            </a>
          ) : (
            <Link
              key={item.href}
              href={item.href}
              className={cn(
                'transition-colors hover:text-foreground/80',
                pathname === item.href ? 'text-foreground' : 'text-foreground/60'
              )}
            >
              {item.title}
            </Link>
          )
        )}
      </nav>
    </div>
  );
};
