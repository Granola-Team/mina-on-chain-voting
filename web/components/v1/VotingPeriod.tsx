import { LinearProgress, Stack, Typography } from '@mui/material';

import type { ProposalParserOutcome } from 'models';
import moment from 'moment';

import { SectionLayout } from './SectionLayout';

export type VotingPeriodProps = {
  startTime: ProposalParserOutcome['start_time'];
  endTime: ProposalParserOutcome['end_time'];
};

export const VotingPeriod = ({ startTime, endTime }: VotingPeriodProps) => {
  const now = moment(new Date()).utc();
  const startDate = moment(new Date(startTime)).utc();
  const endDate = moment(new Date(endTime)).utc();
  const duration = moment.duration(endDate.diff(now));
  const isDone = now.isAfter(endDate);
  const hasNotStarted = now.isBefore(startDate);
  const nowInMillis = moment().valueOf();
  const percentage = ((nowInMillis - startTime) / (endTime - startTime)) * 100;
  const isTBD = startTime === 2000000000000 && endTime === 2000000000000;

  return (
    <SectionLayout>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography fontSize={22} fontWeight={600}>
          Voting Period
        </Typography>
        <Typography variant="body2" fontSize={13} color="hsl(0, 0.8%, 47.1%)">
          Updated at {now.format('YYYY-MM-DD | hh:mm A')} UTC
        </Typography>
      </Stack>

      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
            Start Date
          </Typography>
          <Typography variant="body2" fontSize={17} fontWeight={600}>
            {isTBD ? 'TBD' : `${startDate.format('YYYY-MM-DD | hh:mm A').toString()} UTC`}
          </Typography>
        </Stack>
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="right">
            End Date
          </Typography>
          <Typography variant="body2" fontSize={17} fontWeight={600}>
            {isTBD ? 'TBD' : `${endDate.format('YYYY-MM-DD | hh:mm A').toString()} UTC`}
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
        value={percentage > 100 ? 100 : percentage}
      />
      <Typography variant="subtitle2" fontSize={14} fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
        {isDone
          ? 'The Voting Period has ended.'
          : hasNotStarted
          ? 'The Voting Period has not started yet.'
          : `Remaining time: ${duration.days()}D ${duration.hours()}H ${duration.minutes()}m`}
      </Typography>
    </SectionLayout>
  );
};
