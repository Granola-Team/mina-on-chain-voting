import { LinearProgress, Stack, Typography } from '@mui/material';

import { SectionLayout } from './SectionLayout';

export type VotingPeriodProps = {
  startSlot: number;
  endSlot: number;
  currentSlot: number;
};

export const VotingPeriod = ({ startSlot, endSlot, currentSlot }: VotingPeriodProps) => {
  const current = Math.min(currentSlot, endSlot);

  const distanceFromStart = current - startSlot;
  const totalDistance = endSlot - startSlot;
  const percentage = (distanceFromStart / totalDistance) * 100;

  const hasEnded = currentSlot > endSlot;

  return (
    <SectionLayout>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography fontSize={22} fontWeight={600}>
          Voting Period
        </Typography>
        <Typography variant="body2" fontSize={13} color="hsl(0, 0.8%, 47.1%)">
          Updated at Slot {currentSlot}
        </Typography>
      </Stack>

      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
            Start Slot
          </Typography>
          <Typography variant="body2" fontSize={17} fontWeight={600}>
            {startSlot}
          </Typography>
        </Stack>
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="right">
            End Slot
          </Typography>
          <Typography variant="body2" fontSize={17} fontWeight={600}>
            {endSlot}
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
        value={percentage}
      />
      <Typography variant="subtitle2" fontSize={14} fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
        {hasEnded ? 'The Voting Period has ended.' : `${endSlot - currentSlot} Slots left.`}
      </Typography>
    </SectionLayout>
  );
};
