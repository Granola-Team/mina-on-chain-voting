import { Grid } from '@mui/material';

import { useProposalList } from 'common/store';

import { PageLayout, ProposalCard } from 'components/v1';

const HomePage = () => {
  const [proposals] = useProposalList();

  return (
    <PageLayout>
      <Grid container spacing={1.25}>
        {proposals.map((proposal) => (
          <Grid key={proposal.id} item xs={12} sm={6} md={3}>
            <ProposalCard proposal={proposal} />
          </Grid>
        ))}
      </Grid>
    </PageLayout>
  );
};

export default HomePage;
