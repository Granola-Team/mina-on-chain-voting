'use client';

import * as React from 'react';

import Link from 'next/link';

import { siteConfig } from 'common/config';

export const NavigationDesktop = () => {
  return (
    <div className="mr-4 hidden md:flex">
      <Link href="/" className="mr-6 flex items-center space-x-2">
        <span className="hidden font-bold sm:inline-block">{siteConfig.title}</span>
      </Link>
      <nav className="flex items-center space-x-6 text-sm font-medium">
        {siteConfig.nav.map((item) => (
          <Link
            key={item.href}
            href={item.href}
            className="transition-colors hover:text-foreground/80 text-foreground/60"
          >
            {item.title}
          </Link>
        ))}
      </nav>
    </div>
  );
};
