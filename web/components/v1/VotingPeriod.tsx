import { LinearProgress, Stack, Typography } from '@mui/material';

import { format } from 'date-fns';

export const VotingPeriod = () => {
  const date = new Date(2023, 0, 15, 8, 30, 0);
  const formattedDate = format(date, 'yyyy-MM-dd | hh:mm:ss aa');

  return (
    <Stack width="100%" spacing={1} px={3} py={1} border={1} borderColor="hsl(0, 0%, 24.3%)" borderRadius={2}>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography fontSize={22} fontWeight={600}>
          Voting Period
        </Typography>
        <Typography variant="body2" fontSize={13} color="hsl(0, 0.8%, 47.1%)">
          Updated at {formattedDate}
        </Typography>
      </Stack>

      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
            Start Date
          </Typography>
          <Typography variant="body2" fontWeight={600}>
            {formattedDate}
          </Typography>
        </Stack>
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="right">
            End Date
          </Typography>
          <Typography variant="body2" fontWeight={600}>
            {formattedDate}
          </Typography>
        </Stack>
      </Stack>
      <LinearProgress variant="determinate" sx={{ height: 20, borderRadius: 1.5 }} value={80} />
      <Typography variant="subtitle2" fontSize={14} fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
        The Voting Period has ended.
      </Typography>
    </Stack>
  );
};
