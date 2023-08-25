import { cleanup, render, screen, TestVariant } from 'common/test';

import { VotesMetricsTotalVotes } from 'components/votes-metrics-total-votes';

describe(TestVariant.Component, () => {
  describe(VotesMetricsTotalVotes, () => {
    const data = {
      total: 1234567,
    };

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      render(<VotesMetricsTotalVotes total={data.total} />);
      expect(screen.getByText('Total Votes')).toBeInTheDocument();
    });

    it('renders correct total votes', () => {
      render(<VotesMetricsTotalVotes total={data.total} />);
      expect(screen.getByText('1.234.567')).toBeInTheDocument();
    });

    it('renders correct duplicate disclaimer', () => {
      render(<VotesMetricsTotalVotes total={data.total} />);
      expect(screen.getByText('Duplicates not included')).toBeInTheDocument();
    });
  });
});
