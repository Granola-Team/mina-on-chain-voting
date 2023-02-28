import { LinearProgress, Stack, Typography } from '@mui/material';

import type { CoreApiInfoParserOutcome, ProposalParserOutcome } from 'models';

import { SectionLayout } from './SectionLayout';

export type VotingPeriodProps = {
  startSlot: ProposalParserOutcome['global_start_slot'];
  endSlot: ProposalParserOutcome['global_end_slot'];
  currentSlot: CoreApiInfoParserOutcome['current_slot'];
};

export const VotingPeriod = ({ startSlot, endSlot, currentSlot }: VotingPeriodProps) => {
  const current = currentSlot < endSlot ? currentSlot : endSlot;

  const distanceFromStart = current - startSlot;
  const totalDistance = endSlot - startSlot;
  const percentage = (distanceFromStart / totalDistance) * 100n;

  const hasEnded = currentSlot > endSlot;

  return (
    <SectionLayout>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography fontSize={22} fontWeight={600}>
          Voting Period
        </Typography>
        <Typography variant="body2" fontSize={13} color="hsl(0, 0.8%, 47.1%)">
          Updated at Slot {currentSlot.toString()}
        </Typography>
      </Stack>

      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
            Start Slot
          </Typography>
          <Typography variant="body2" fontSize={17} fontWeight={600}>
            {startSlot.toString()}
          </Typography>
        </Stack>
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="right">
            End Slot
          </Typography>
          <Typography variant="body2" fontSize={17} fontWeight={600}>
            {endSlot.toString()}
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
        {hasEnded ? 'The Voting Period has ended.' : `${endSlot - currentSlot} Slots left.`}
      </Typography>
    </SectionLayout>
  );
};
