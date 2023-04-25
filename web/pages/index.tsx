import { Grid, Stack } from '@mui/material';

import { useProposalList } from 'common/store';

import { PageLayout, ProposalCard } from 'components/v1';
import { IntroOverview } from 'components/v1/IntroOverview';

const HomePage = () => {
  const [proposals] = useProposalList();

  return (
    <PageLayout>
      <Stack direction="row">
        <IntroOverview />
      </Stack>
      <Grid container>
        {proposals.map((proposal) => (
          <Grid key={proposal.id} item xs sm md>
            <ProposalCard proposal={proposal} />
          </Grid>
        ))}
      </Grid>
    </PageLayout>
  );
};

export default HomePage;
