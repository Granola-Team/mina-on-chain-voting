import type { GetServerSidePropsContext, InferGetServerSidePropsType } from 'next';

import { Stack } from '@mui/material';

import { minaProposalIdAtom, useMinaProposal } from 'common/store';

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
  useHydrateAtoms([[minaProposalIdAtom, props.proposalId]]);
  const [proposal] = useMinaProposal();

  return (
    <PageLayout>
      <Stack direction="row" spacing={1}>
        <Instructions keyword={proposal.key} />
        <TotalVotes totalVotes={proposal.votes.length} />
      </Stack>
      <VotingPeriod
        startDate={new Date(2022, 0, 15, 8, 30, 0)}
        endDate={new Date(2023, 0, 15, 8, 30, 0)}
        queryDate={new Date(2023, 0, 15, 8, 30, 0)}
      />
      <VotesTable votes={proposal.votes} />
    </PageLayout>
  );
};

export default ProposalPage;
