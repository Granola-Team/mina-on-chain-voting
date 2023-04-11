import type { GetServerSidePropsContext, InferGetServerSidePropsType } from 'next';

import { Stack } from '@mui/material';

import { proposalIdAtom, useProposal } from 'common/store';

import { Instructions, PageLayout, TotalVotes, VotesTable, VotingPeriod } from 'components/v1';

import { useHydrateAtoms } from 'jotai/react/utils';

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
    <PageLayout title={proposal.title ? `${proposal.key}: ${proposal.title}` : undefined}>
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
