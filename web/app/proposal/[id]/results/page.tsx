import { getProposalResults } from 'common/store';

import { PageHeader, PageHeaderDescription, PageHeaderHeading, SmallerPageHeaderDescription } from 'components/core/page-header';
import { Separator } from 'components/core/separator';
import { VotesMetrics } from 'components/votes-metrics';
import { VotesTable } from 'components/votes-table';

interface PageParams {
  id: string;
}

const Page = async ({ params }: { params: PageParams }) => {
  const proposal = await getProposalResults(params.id);

  return (
    <div className="container relative">
      <PageHeader className="hidden md:block pb-6">
        <PageHeaderHeading>{proposal.title}</PageHeaderHeading>
        <PageHeaderDescription>{proposal.description}</PageHeaderDescription>
        <SmallerPageHeaderDescription className="text-base max-w-[2000px]">
          Authoritative document: {' '}
          <a
            href={proposal.url}
            target="_blank"
            rel="noopener noreferrer"
            className="text-blue-500"
          >
            {proposal.url}
          </a>
         </SmallerPageHeaderDescription>
      </PageHeader>

      <PageHeader className="block md:hidden pb-6 text-center">
        <PageHeaderHeading className="text-2xl">{proposal.title}</PageHeaderHeading>
        <PageHeaderDescription className="text-base">{proposal.description}</PageHeaderDescription>
        <SmallerPageHeaderDescription className="text-base max-w-[2000px]">
          Authoritative document: {' '}
            <a
              href={proposal.url}
              target="_blank"
              rel="noopener noreferrer"
              className="text-blue-500"
            >
              {proposal.url}
            </a>
        </SmallerPageHeaderDescription>
      </PageHeader>

      <div className="flex flex-col gap-2">
        <VotesMetrics variant="results" proposal={proposal} />
        <Separator />
        <VotesTable votes={proposal.votes} />
      </div>
    </div>
  );
};

export default Page;
