import { GetProposalResult, GetProposalResultsResult } from 'common/store';
import { cleanup, render, screen, TestVariant } from 'common/test';

import { VotesMetrics } from 'components/votes-metrics';

import { mockDeep } from 'jest-mock-extended';

jest.mock('recharts', () => ({
  ...jest.requireActual('recharts'),
  ResponsiveContainer: (props: React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>) => (
    <div {...props} />
  ),
}));

describe(TestVariant.Component, () => {
  describe(VotesMetrics, () => {
    const pendingProposal = mockDeep<GetProposalResult>({
      key: 'MIP1',
      status: 'Pending',
      start_time: 1698253600000,
      end_time: 1798253600000,
    });

    const resultsProposal = mockDeep<GetProposalResultsResult>({
      key: 'MIP1',
      status: 'Completed',
      start_time: 1698253600000,
      end_time: 1798253600000,
    });

    afterEach(() => {
      cleanup();
    });

    describe('renders component', () => {
      it('default', () => {
        render(<VotesMetrics variant="default" proposal={pendingProposal} />);
        expect(screen.getByText('Voting Distribution')).toBeInTheDocument();
        expect(screen.getByText('Total Votes')).toBeInTheDocument();
        expect(screen.getByText('How do I cast my vote?')).toBeInTheDocument();
        expect(screen.getByText('Voting Period')).toBeInTheDocument();
      });

      it('results', () => {
        render(<VotesMetrics variant="results" proposal={resultsProposal} />);
        expect(screen.getByText('Voting Distribution')).toBeInTheDocument();
        expect(screen.getByText('Total Votes')).toBeInTheDocument();
        expect(screen.getByText('Total Stake Participated')).toBeInTheDocument();
        expect(screen.getByText('Voting Results')).toBeInTheDocument();
      });
    });
  });
});
