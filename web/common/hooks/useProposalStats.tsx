import { useCallback } from 'react';

import type { ProposalResultsParserOutcome } from 'models';

export const useProposalStats = (votes: ProposalResultsParserOutcome['votes']) => {
  const stakeWeight = useCallback(() => {
    let positiveStakeWeight = 0;
    let negativeStakeWeight = 0;

    votes.forEach((vote) => {
      const weight = Number.parseFloat(vote.weight);

      if (weight > 0) {
        if (vote.memo.startsWith('no ')) {
          negativeStakeWeight += weight;
        } else {
          positiveStakeWeight += weight;
        }
      }
    });

    const totalStakeWeight = positiveStakeWeight + negativeStakeWeight;

    return { positiveStakeWeight, negativeStakeWeight, totalStakeWeight };
  }, [votes]);

  const createPercent = (value: number) => {
    const _value = (value / stakeWeight().totalStakeWeight) * 100;

    if (Number.isNaN(_value)) {
      return undefined;
    }

    return _value.toFixed(2);
  };

  const positivePercentage = createPercent(stakeWeight().positiveStakeWeight);
  const negativePercentage = createPercent(stakeWeight().negativeStakeWeight);

  return { stakeWeight, positivePercentage, negativePercentage, createPercent };
};
