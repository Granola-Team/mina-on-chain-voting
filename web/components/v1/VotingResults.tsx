import { LinearProgress, Stack, Typography } from '@mui/material';

import { SectionLayout } from './SectionLayout';

export type VotingResultsProps = {
  positivePercentage: string | undefined;
  negativePercentage: string | undefined;
};

export const VotingResults = ({ positivePercentage, negativePercentage }: VotingResultsProps) => {
  return (
    <SectionLayout>
      <Typography fontSize={22} fontWeight={600} mb="-6px">
        Voting Results
      </Typography>

      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="left">
            Yes
          </Typography>
          <Typography variant="body1" fontSize={17} fontWeight={600}>
            {positivePercentage}%
          </Typography>
        </Stack>
        <Stack>
          <Typography variant="body2" fontWeight={500} color="hsl(0, 0.8%, 47.1%)" textAlign="right">
            No
          </Typography>
          <Typography variant="body1" fontSize={17} fontWeight={600}>
            {negativePercentage}%
          </Typography>
        </Stack>
      </Stack>

      <LinearProgress
        variant="determinate"
        sx={{
          height: 20,
          borderRadius: 1.5,
          '& .MuiLinearProgress-bar1Determinate': {
            backgroundColor: '#66bb6a',
          },
          '&.MuiLinearProgress-colorPrimary': {
            backgroundColor: '#ff0000a6',
          },
        }}
        value={Number.parseFloat(positivePercentage ?? '0.00')}
      />
    </SectionLayout>
  );
};
