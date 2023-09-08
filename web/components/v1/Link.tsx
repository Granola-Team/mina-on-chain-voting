import NextLink, { LinkProps } from 'next/link';

import { useTheme } from 'components/provider';

export const Link = ({ children, ...props }: LinkProps & React.PropsWithChildren) => {
  const { theme } = useTheme();

  return (
    <NextLink
      style={{ textDecoration: 'none', color: theme.key === 'light' ? '#1C1C1C' : 'hsl(0, 0%, 100%)' }}
      {...props}
    >
      {children}
    </NextLink>
  );
};
