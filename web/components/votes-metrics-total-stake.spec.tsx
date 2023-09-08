import { cleanup, render, screen, TestVariant } from 'common/test';

import { VotesMetricsTotalStake } from 'components/votes-metrics-total-stake';

describe(TestVariant.Component, () => {
  describe(VotesMetricsTotalStake, () => {
    const data = {
      total: 1234.5678,
    };

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      render(<VotesMetricsTotalStake total={data.total} />);
      expect(screen.getByText('Total Stake Participated')).toBeInTheDocument();
    });

    it('renders correct total stake', () => {
      render(<VotesMetricsTotalStake total={data.total} />);
      expect(screen.getByText('1,234.57')).toBeInTheDocument();
    });

    it('renders correct error margin', () => {
      render(<VotesMetricsTotalStake total={data.total} />);
      expect(screen.getByText('± 0.005')).toBeInTheDocument();
    });
  });
});
