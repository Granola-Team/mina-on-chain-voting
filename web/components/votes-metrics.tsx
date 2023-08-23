'use client';

import { GetProposalResult, GetProposalResultsResult } from 'common/store';

import {
  VotingDistribution,
  VotingInstructions,
  VotingPeriod,
  VotingResults,
  VotingTotal,
  VotingTotalStake,
} from 'components/votes-metrics-cards';

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
    <div className="grid grid-cols-5 grid-rows-2 gap-1.5">
      <VotingDistribution data={proposal.metrics} />
      <VotingTotal total={proposal.votes.length} />

      {variant === 'results' && proposal.status === 'Completed' ? (
        <VotingTotalStake total={proposal.total_stake_weight} />
      ) : (
        <VotingInstructions memo={proposal.key} />
      )}

      {variant === 'results' && proposal.status === 'Completed' ? (
        <VotingResults
          total={proposal.total_stake_weight}
          positive={proposal.positive_stake_weight}
          negative={proposal.negative_stake_weight}
        />
      ) : (
        <VotingPeriod startTime={proposal.start_time} endTime={proposal.end_time} status={proposal.status} />
      )}
    </div>
  );
};
