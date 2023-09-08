import { Stack, StackProps } from '@mui/material';

import { useTheme } from 'components/provider';

export type SectionLayoutProps = React.PropsWithChildren & StackProps;

export const SectionLayout = ({ width = '100%', spacing = 1, children, ...props }: SectionLayoutProps) => {
  const { theme } = useTheme();

  return (
    <Stack
      width={width}
      spacing={spacing}
      px={3}
      py={1}
      border={1}
      borderColor="hsl(0, 0%, 24.3%)"
      borderRadius={2}
      sx={{ backgroundColor: theme.key === 'dark' ? '#1C1C1C' : 'hsl(0, 0%, 100%)' }}
      {...props}
    >
      {children}
    </Stack>
  );
};
