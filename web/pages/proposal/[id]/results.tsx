import type { GetServerSidePropsContext, InferGetServerSidePropsType } from 'next';

import { useProposalStats } from 'common/hooks';
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

  const { positivePercentage, negativePercentage, createPercent } = useProposalStats(proposal.votes);

  return (
    <PageLayout>
      <ResultsOverview />
      <VotingPeriod
        startSlot={proposal.global_start_slot}
        endSlot={proposal.global_end_slot}
        currentSlot={info.current_slot}
      />
      <VotingResults {...{ positivePercentage, negativePercentage }} />
      <ResultsTable {...{ votes: proposal.votes, createPercent }} />
    </PageLayout>
  );
};

export default ProposalResultsPage;
