import { Button, Stack, Typography } from '@mui/material';

import { useTheme } from 'components/provider';

export default function Home() {
  const { theme, setTheme } = useTheme();

  return (
    <Stack spacing={3} height="100vh" width="100%" display="flex" justifyContent="center" alignItems="center">
      <Typography variant="h2">Hello from {process.env.NEXT_PUBLIC_RELEASE_STAGE}!</Typography>
      <Button
        variant="contained"
        onClick={() => {
          setTheme(theme.key === 'dark' ? 'light' : 'dark');
        }}
      >
        Toggle Theme
      </Button>
    </Stack>
  );
}
