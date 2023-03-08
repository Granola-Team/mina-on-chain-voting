import { Stack, Tooltip, Typography } from '@mui/material';

import { SectionLayout } from './SectionLayout';

export type TotalVotesProps = {
  totalVotes: number;
};

export const TotalVotes = ({ totalVotes }: TotalVotesProps) => {
  return (
    <SectionLayout width="15%">
      <Stack height="100%" justifyContent="center" alignItems="center">
        <Tooltip
          title="Sum of unique accounts who have submitted a valid vote during the voting period"
          placement="top-end"
        >
          <Typography variant="h6" fontWeight={600}>
            Total Votes
          </Typography>
        </Tooltip>
        <Typography variant="h5" fontWeight={600}>
          {totalVotes}
        </Typography>
      </Stack>
    </SectionLayout>
  );
};
