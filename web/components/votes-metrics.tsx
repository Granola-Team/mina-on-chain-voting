'use client';

import { GetProposalResult, GetProposalResultsResult } from 'common/store';

import { VotesMetricsChart } from 'components/votes-metrics-chart';
import { VotesMetricsInstructions } from 'components/votes-metrics-instructions';
import { VotesMetricsPeriod } from 'components/votes-metrics-period';
import { VotesMetricsResults } from 'components/votes-metrics-results';
import { VotesMetricsTotalStake } from 'components/votes-metrics-total-stake';
import { VotesMetricsTotalVotes } from 'components/votes-metrics-total-votes';

type Variants =
  | {
      variant: 'default';
      proposal: GetProposalResult;
    }
  | {
      variant: 'results';
      proposal: GetProposalResultsResult;
    };

type Props = Variants;

export const VotesMetrics = ({ variant, proposal }: Props) => {
  return (
    <div className="grid gap-1.5 grid-cols-2 grid-rows-4 md:grid-cols-4 md:grid-rows-3 xl:grid-cols-5 xl:grid-rows-2">
      <VotesMetricsChart className="row-span-2 col-span-2 md:col-span-4 xl:col-span-3" data={proposal.metrics} />

      <VotesMetricsTotalVotes
        className="row-start-4 md:row-start-3 xl:row-start-1 xl:col-start-4"
        total={proposal.votes.length}
      />

      {variant === 'results' && proposal.status === 'Completed' ? (
        <VotesMetricsTotalStake
          className="row-start-4 md:row-start-3 xl:row-start-1 xl:col-start-5"
          total={proposal.total_stake_weight}
        />
      ) : (
        <VotesMetricsInstructions
          className="row-start-4 md:row-start-3 xl:row-start-1 xl:col-start-5"
          memo={proposal.key}
        />
      )}

      {variant === 'results' && proposal.status === 'Completed' ? (
        <VotesMetricsResults
          className="col-span-2 md:row-start-3 xl:row-start-2 xl:col-start-4"
          total={proposal.total_stake_weight}
          positive={proposal.positive_stake_weight}
          negative={proposal.negative_stake_weight}
        />
      ) : (
        <VotesMetricsPeriod
          className="col-span-2 md:row-start-3 xl:row-start-2 xl:col-start-4"
          startTime={proposal.start_time}
          endTime={proposal.end_time}
          status={proposal.status}
        />
      )}
    </div>
  );
};
