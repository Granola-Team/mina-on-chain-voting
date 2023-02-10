import { Stack } from '@mui/material';

import { useTheme } from 'components/provider';

export type SectionLayoutProps = React.PropsWithChildren & {
  width?: string;
};

export const SectionLayout = ({ width = '100%', children }: SectionLayoutProps) => {
  const { theme } = useTheme();

  return (
    <Stack
      width={width}
      spacing={1}
      px={3}
      py={1}
      border={1}
      borderColor="hsl(0, 0%, 24.3%)"
      borderRadius={2}
      sx={{ backgroundColor: theme.key === 'dark' ? 'hsl(0, 0%, 8.5%)' : 'hsl(0, 0%, 100%)' }}
    >
      {children}
    </Stack>
  );
};
