import { cleanup, render, screen, TestVariant } from 'common/test';

import { VotesMetricsChart } from 'components/votes-metrics-chart';

jest.mock('recharts', () => ({
  ...jest.requireActual('recharts'),
  ResponsiveContainer: (props: React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>) => (
    <div {...props} />
  ),
}));

describe(TestVariant.Component, () => {
  describe(VotesMetricsChart, () => {
    const mockData = [
      { FOR: 10, AGAINST: 5, DATE: '2021-01-01' },
      { FOR: 20, AGAINST: 10, DATE: '2021-01-02' },
    ];

    beforeEach(() => {
      render(<VotesMetricsChart data={mockData} />);
    });

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      expect(screen.getByText('Voting Distribution')).toBeInTheDocument();
      expect(screen.getByText(/Track the distribution/)).toBeInTheDocument();
    });
  });
});
