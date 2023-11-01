import { Suspense } from 'react';

import { getProposal } from 'common/store';

import { PageHeader, PageHeaderDescription, PageHeaderHeading } from 'components/core/page-header';
import { Separator } from 'components/core/separator';
import { VotesMetrics } from 'components/votes-metrics';
import { VotesTable } from 'components/votes-table';

import PageLoading from './results/loading';

interface PageParams {
  id: string;
}

const Page = async ({ params }: { params: PageParams }) => {
  const proposal = await getProposal(params.id);

  return (
    <div className="container relative">
      <Suspense fallback={<PageLoading/>}>
        <PageHeader className="hidden md:block pb-6">
          <PageHeaderHeading>{proposal.title}</PageHeaderHeading>
          <PageHeaderDescription>{proposal.description}</PageHeaderDescription>
        </PageHeader>

        <PageHeader className="block md:hidden pb-6 text-center">
          <PageHeaderHeading className="text-2xl">{proposal.title}</PageHeaderHeading>
          <PageHeaderDescription className="text-base">{proposal.description}</PageHeaderDescription>
        </PageHeader>

        <div className="flex flex-col gap-2">
          <VotesMetrics variant="default" proposal={proposal} />
          <Separator />
          <VotesTable votes={proposal.votes} />
        </div>
      </Suspense>
    </div>
  );
};

export default Page;
