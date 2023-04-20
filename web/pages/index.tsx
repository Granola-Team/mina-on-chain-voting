import { Grid, Stack } from '@mui/material';

import { useProposalList } from 'common/store';

import { PageLayout, ProposalCard } from 'components/v1';
import { IntroOverview } from 'components/v1/IntroOverview';

const HomePage = () => {
  const [proposals] = useProposalList();

  return (
    <PageLayout>
      <Stack direction="row" spacing={1}>
        <IntroOverview/>
      </Stack>
      <Grid container spacing={0} justifyContent="flex-end">
        {proposals.map((proposal) => (
          <Grid key={proposal.id} item xs={12} sm={6} md={4}>
            <ProposalCard proposal={proposal} />
          </Grid>
        ))}
      </Grid>
    </PageLayout>
  );
};

export default HomePage;
