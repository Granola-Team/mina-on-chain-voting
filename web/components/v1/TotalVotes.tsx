import { Stack, Typography } from '@mui/material';

export const TotalVotes = () => {
  return (
    <Stack
      minWidth="220px"
      justifyContent="center"
      alignItems="center"
      border={1}
      borderColor="hsl(0, 0%, 24.3%)"
      borderRadius={2}
    >
      <Typography variant="h6" fontWeight={600}>
        Total Votes
      </Typography>
      <Typography variant="h6" fontWeight={600}>
        0
      </Typography>
    </Stack>
  );
};
