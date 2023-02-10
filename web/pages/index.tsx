import { Stack } from '@mui/material';

import { Instructions, PageLayout, TotalVotes, VotesTable, VotingPeriod } from 'components/v1';

const HomePage = () => {
  return (
    <PageLayout>
      <Stack direction="row" spacing={1.25}>
        <Instructions />
        <TotalVotes />
      </Stack>
      <VotingPeriod />
      <VotesTable />
    </PageLayout>
  );
};

export default HomePage;
