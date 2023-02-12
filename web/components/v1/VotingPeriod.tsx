import { LinearProgress, Stack, Typography } from '@mui/material';

import { format } from 'date-fns';

import { SectionLayout } from './SectionLayout';

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
    <SectionLayout>
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
          <Typography variant="body2" fontSize={17} fontWeight={600}>
            {formattedStartDate}
          </Typography>
        </Stack>
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="right">
            End Date
          </Typography>
          <Typography variant="body2" fontSize={17} fontWeight={600}>
            {formattedEndDate}
          </Typography>
        </Stack>
      </Stack>
      <LinearProgress
        variant="determinate"
        sx={{
          height: 20,
          borderRadius: 1.5,
          '& .MuiLinearProgress-bar1Determinate': {
            background: 'linear-gradient(to right, rgb(96 165 250 / .8), #7c3aed)',
          },
          '&.MuiLinearProgress-colorPrimary': {
            backgroundColor: '#570ddb8f',
          },
        }}
        value={80}
      />
      <Typography variant="subtitle2" fontSize={14} fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
        The Voting Period has ended.
      </Typography>
    </SectionLayout>
  );
};
