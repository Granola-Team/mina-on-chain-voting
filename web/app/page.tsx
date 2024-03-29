import { getProposalList } from 'common/store';

import { PageHeader, PageHeaderDescription, PageHeaderHeading } from 'components/core/page-header';
import { ProposalTable } from 'components/proposal-table';

export const dynamic = 'force-dynamic';

const Page = async () => {
  const proposals = await getProposalList();

  return (
    <div className="container relative">
      <PageHeader className="hidden md:block pb-6">
        <PageHeaderHeading>Mina On-Chain Voting</PageHeaderHeading>
        <PageHeaderDescription>View and track the progress of Mina Improvement Proposals (MIPs).</PageHeaderDescription>
      </PageHeader>

      <PageHeader className="block md:hidden pb-6 text-center">
        <PageHeaderHeading className="text-2xl">Mina On-Chain Voting</PageHeaderHeading>
        <PageHeaderDescription>View and track the progress of Mina Improvement Proposals (MIPs).</PageHeaderDescription>
      </PageHeader>

      <ProposalTable proposals={proposals} />
    </div>
  );
};

export default Page;
