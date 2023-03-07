import { LinearProgress, Stack, Typography } from '@mui/material';

import type { ProposalParserOutcome } from 'models';
import moment from 'moment';

import { SectionLayout } from './SectionLayout';

export type VotingPeriodProps = {
  startSlot: ProposalParserOutcome['global_start_slot'];
  endSlot: ProposalParserOutcome['global_end_slot'];
};

export const VotingPeriod = ({ startSlot, endSlot }: VotingPeriodProps) => {
  const now = moment(new Date()).utc();
  const startDate = moment(new Date(startSlot)).utc();
  const endDate = moment(new Date(endSlot)).utc();
  const duration = moment.duration(endDate.diff(now));
  const isDone = now.isAfter(endDate);
  const hasNotStarted = now.isBefore(startDate);

  const nowInMillis = moment().valueOf();

  const percentage = ((nowInMillis - startSlot) / (endSlot - startSlot)) * 100;

  return (
    <SectionLayout>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography fontSize={22} fontWeight={600}>
          Voting Period
        </Typography>
        <Typography variant="body2" fontSize={13} color="hsl(0, 0.8%, 47.1%)">
          Updated at {now.format("YYYY-MM-DD | hh:mm:ss A")} UTC
        </Typography>
      </Stack>

      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
            Start Date
          </Typography>
          <Typography variant="body2" fontSize={17} fontWeight={600}>
          {startDate.format("YYYY-MM-DD | hh:mm:ss A").toString()} UTC
          </Typography>
        </Stack>
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="right">
            End Date
          </Typography>
          <Typography variant="body2" fontSize={17} fontWeight={600}>
          {endDate.format("YYYY-MM-DD | hh:mm:ss A").toString()} UTC
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
        value={Number(percentage)}
      />
      <Typography variant="subtitle2" fontSize={14} fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
      {isDone
                ? "The Voting Period has ended."
                : hasNotStarted
                ? "The Voting Period has not started yet."
                : `Remaining time: ${duration.days()}D ${duration.hours()}H ${duration.minutes()}m`}
      </Typography>
    </SectionLayout>
  );
};
