import type { GetServerSidePropsContext, InferGetServerSidePropsType } from 'next';

import { Stack } from '@mui/material';

import { proposalIdAtom, useCoreApiInfo, useProposal } from 'common/store';

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
  const [info] = useCoreApiInfo();

  return (
    <PageLayout>
      <Stack direction="row" spacing={1}>
        <Instructions keyword={proposal.key} />
        <TotalVotes totalVotes={proposal.votes.length} />
      </Stack>
      <VotingPeriod
        startSlot={proposal.global_start_slot}
        endSlot={proposal.global_end_slot}
        currentSlot={info.current_slot}
      />
      <VotesTable votes={proposal.votes} />
    </PageLayout>
  );
};

export default ProposalPage;
