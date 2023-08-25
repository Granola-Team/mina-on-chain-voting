import { cleanup, render, screen, TestVariant } from 'common/test';

import { VotesMetricsPeriod } from 'components/votes-metrics-period';

import { ProposalStatus } from 'models';

describe(TestVariant.Component, () => {
  describe(VotesMetricsPeriod, () => {
    const pending = {
      status: 'Pending' as ProposalStatus,
      start_time: 1698253600000,
      end_time: 1798253600000,
    };

    const inProgress = {
      status: 'In Progress' as ProposalStatus,
      start_time: 1645253600000,
      end_time: 1698753600000,
    };

    const inReview = {
      status: 'In Review' as ProposalStatus,
      start_time: 1684562400000,
      end_time: 1685253600000,
    };

    const completed = {
      status: 'Completed' as ProposalStatus,
      start_time: 1672848000000,
      end_time: 1673685000000,
    };

    afterEach(() => {
      cleanup();
    });

    it('renders component', () => {
      render(<VotesMetricsPeriod startTime={0} endTime={1} status="Unknown" />);
      expect(screen.getByText('Voting Period')).toBeInTheDocument();
    });

    describe('renders correct status', () => {
      it('Pending', () => {
        render(
          <VotesMetricsPeriod startTime={pending.start_time} endTime={pending.end_time} status={pending.status} />
        );

        expect(screen.getByText('Voting has not started yet')).toBeInTheDocument();
      });

      it('In Progress', () => {
        render(
          <VotesMetricsPeriod
            startTime={inProgress.start_time}
            endTime={inProgress.end_time}
            status={inProgress.status}
          />
        );

        expect(screen.getByText(/Remaining time:/)).toBeInTheDocument();
      });

      it('In Review', () => {
        render(
          <VotesMetricsPeriod startTime={inReview.start_time} endTime={inReview.end_time} status={inReview.status} />
        );

        expect(screen.getByText('Results are being verified')).toBeInTheDocument();
      });

      it('Completed', () => {
        render(
          <VotesMetricsPeriod startTime={completed.start_time} endTime={completed.end_time} status={completed.status} />
        );

        expect(screen.getByText('Voting has ended')).toBeInTheDocument();
      });
    });

    it('renders progress bar with correct value', () => {
      render(<VotesMetricsPeriod startTime={completed.start_time} endTime={completed.end_time} status="Completed" />);
      const progressBar = screen.getByRole('progressbar');
      expect(progressBar).toBeInTheDocument();

      const innerDiv = progressBar.querySelector('div');
      expect(innerDiv).not.toBeNull();
      expect(innerDiv).toHaveStyle('transform: translateX(-0%)');
    });
  });
});
