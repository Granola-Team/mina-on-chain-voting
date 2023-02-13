import { Stack, Typography } from '@mui/material';

import { SectionLayout } from './SectionLayout';

export type TotalVotesProps = {
  totalVotes: number;
};

export const TotalVotes = ({ totalVotes }: TotalVotesProps) => {
  return (
    <SectionLayout width="15%">
      <Stack height="100%" justifyContent="center" alignItems="center">
        <Typography variant="h6" fontWeight={600}>
          Total Votes
        </Typography>
        <Typography variant="h5" fontWeight={600}>
          {totalVotes}
        </Typography>
      </Stack>
    </SectionLayout>
  );
};
