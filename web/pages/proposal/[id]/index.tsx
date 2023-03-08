import type { GetServerSidePropsContext, InferGetServerSidePropsType } from 'next';
// Use next dynamic to import VotingPeriod and disable ssr
import dynamic from 'next/dynamic';

import { Stack } from '@mui/material';

import { proposalIdAtom, useProposal } from 'common/store';

import { Instructions, PageLayout, TotalVotes, VotesTable } from 'components/v1';

import { useHydrateAtoms } from 'jotai/react/utils';

const VotingPeriod = dynamic(() => import('components/v1/VotingPeriod').then((mod) => mod.VotingPeriod), {
  ssr: false,
});

type ProposalPageProps = {
  id: string;
};

export const getServerSideProps = async (context: GetServerSidePropsContext<ProposalPageProps>) => {
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

const ProposalPage = (props: InferGetServerSidePropsType<typeof getServerSideProps>) => {
  useHydrateAtoms([[proposalIdAtom, props.proposalId]]);
  const [proposal] = useProposal();

  return (
    <PageLayout>
      <Stack direction="row" spacing={1}>
        <Instructions keyword={proposal.key} />
        <TotalVotes totalVotes={proposal.votes.length} />
      </Stack>
      <VotingPeriod startTime={proposal.start_time} endTime={proposal.end_time} />
      <VotesTable votes={proposal.votes} />
    </PageLayout>
  );
};

export default ProposalPage;
