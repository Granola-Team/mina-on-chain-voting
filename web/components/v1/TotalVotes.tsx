import { Stack, Tooltip, Typography } from '@mui/material';

import HelpOutlineIcon from '@material-ui/icons/HelpOutline';

import { SectionLayout } from './SectionLayout';

export type TotalVotesProps = {
  totalVotes: number;
};

export const TotalVotes = ({ totalVotes }: TotalVotesProps) => {
  return (
    <SectionLayout width="15%">
      <Stack height="100%" justifyContent="center" alignItems="center">
        <Stack spacing={0.25} direction="row" justifyContent="center" alignItems="center">
          <Typography variant="h6" fontWeight={600}>
            Total Votes
          </Typography>

          <Tooltip
            arrow
            title={
              <Typography fontSize={12}>
                Total Votes is the sum of all valid votes per unique account that were received during the voting
                period. Duplicate votes are not included in the total.
              </Typography>
            }
          >
            <HelpOutlineIcon fontSize="small" />
          </Tooltip>
        </Stack>
        <Typography variant="h5" fontWeight={600}>
          {totalVotes}
        </Typography>
      </Stack>
    </SectionLayout>
  );
};
