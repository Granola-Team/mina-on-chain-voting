import { cleanup, render, screen, TestVariant } from 'common/test';

import { VotesMetricsResults } from 'components/votes-metrics-results';

describe(TestVariant.Component, () => {
  describe(VotesMetricsResults, () => {
    const data = {
      total: 1000,
      positive: 700,
      negative: 300,
    };

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      render(<VotesMetricsResults total={data.total} positive={data.positive} negative={data.negative} />);
      expect(screen.getByText('Voting Results')).toBeInTheDocument();
    });

    it('renders correct percentages', () => {
      render(<VotesMetricsResults total={data.total} positive={data.positive} negative={data.negative} />);
      expect(screen.getByText('FOR - 70%')).toBeInTheDocument();
      expect(screen.getByText('AGAINST - 30%')).toBeInTheDocument();
    });

    it('renders 0% in case of zero total', () => {
      render(<VotesMetricsResults total={0} positive={0} negative={0} />);
      expect(screen.getByText('FOR - 0%')).toBeInTheDocument();
      expect(screen.getByText('AGAINST - 0%')).toBeInTheDocument();
    });

    it('renders progress bar with correct value', () => {
      render(<VotesMetricsResults total={data.total} positive={data.positive} negative={data.negative} />);
      const progressBar = screen.getByRole('progressbar');
      expect(progressBar).toBeInTheDocument();

      const innerDiv = progressBar.querySelector('div');
      expect(innerDiv).not.toBeNull();
      expect(innerDiv).toHaveStyle('transform: translateX(-30%)');
    });
  });
});
