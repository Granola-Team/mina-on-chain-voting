import type { GetServerSidePropsContext, InferGetServerSidePropsType } from 'next';

import { proposalIdAtom, useCoreApiInfo, useProposalResults } from 'common/store';

import { PageLayout, ResultsOverview, ResultsTable, VotingPeriod, VotingResults } from 'components/v1';

import { useHydrateAtoms } from 'jotai/react/utils';

type ProposalResultsPageProps = {
  id: string;
};

export const getServerSideProps = async (context: GetServerSidePropsContext<ProposalResultsPageProps>) => {
  const proposalIdParam = context.params?.id;
  const proposalId = Number(proposalIdParam);

  if (!proposalIdParam || isNaN(proposalId)) {
    return {
      notFound: true,
    };
  }

  return {
    props: {
      proposalId,
    },
  };
};

const ProposalResultsPage = (props: InferGetServerSidePropsType<typeof getServerSideProps>) => {
  useHydrateAtoms([[proposalIdAtom, props.proposalId]]);
  const [proposal] = useProposalResults();
  const [info] = useCoreApiInfo();

  return (
    <PageLayout>
      <ResultsOverview />
      <VotingPeriod
        startSlot={proposal.global_start_slot}
        endSlot={proposal.global_end_slot}
        currentSlot={info.current_slot}
      />
      <VotingResults
        total={proposal.total_stake_weight}
        positive={proposal.positive_stake_weight}
        negative={proposal.negative_stake_weight}
      />
      <ResultsTable {...{ votes: proposal.votes, totalStakeWeight: proposal.total_stake_weight }} />
    </PageLayout>
  );
};

export default ProposalResultsPage;
