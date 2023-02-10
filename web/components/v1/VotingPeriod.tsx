import { LinearProgress, Stack, Typography } from '@mui/material';

import { format } from 'date-fns';

export type VotingPeriodProps = {
  startDate: Date;
  endDate: Date;
  queryDate: Date;
};

export const VotingPeriod = ({ startDate, endDate, queryDate }: VotingPeriodProps) => {
  const formattedQueryDate = format(queryDate, 'yyyy-MM-dd | hh:mm:ss aa');
  const formattedStartDate = format(startDate, 'yyyy-MM-dd | hh:mm:ss aa');
  const formattedEndDate = format(endDate, 'yyyy-MM-dd | hh:mm:ss aa');

  return (
    <Stack width="100%" spacing={1} px={3} py={1} border={1} borderColor="hsl(0, 0%, 24.3%)" borderRadius={2}>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography fontSize={22} fontWeight={600}>
          Voting Period
        </Typography>
        <Typography variant="body2" fontSize={13} color="hsl(0, 0.8%, 47.1%)">
          Updated at {formattedQueryDate}
        </Typography>
      </Stack>

      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
            Start Date
          </Typography>
          <Typography variant="body2" fontWeight={600}>
            {formattedStartDate}
          </Typography>
        </Stack>
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="right">
            End Date
          </Typography>
          <Typography variant="body2" fontWeight={600}>
            {formattedEndDate}
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
