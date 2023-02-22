import { PageLayout, ResultsOverview, ResultsTable, VotingPeriod, VotingResults } from 'components/v1';

//! Dummy Data
const startDate = new Date(2023, 0, 15, 8, 30, 0);
const endDate = new Date(2023, 4, 15, 8, 30, 0);
const queryDate = new Date(2023, 1, 10, 8, 30, 0);

const useStakeStatistics = (votes: any) => {
  const stakeWeight = () => {
    let positiveStakeWeight = 0;
    let negativeStakeWeight = 0;

    votes.forEach((vote: any) => {
      if (vote.stake_weight > 0) {
        if (vote.memo.startsWith('no ')) {
          negativeStakeWeight += vote.stake_weight;
        } else {
          positiveStakeWeight += vote.stake_weight;
        }
      }
    });

    const totalStakeWeight = positiveStakeWeight + negativeStakeWeight;

    return { positiveStakeWeight, negativeStakeWeight, totalStakeWeight };
  };

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

const ProposalResultsPage = () => {
  const { positivePercentage, negativePercentage, createPercent } = useStakeStatistics(data);

  return (
    <PageLayout>
      <ResultsOverview />
      <VotingPeriod {...{ startDate, endDate, queryDate }} />
      <VotingResults {...{ positivePercentage, negativePercentage }} />
      <ResultsTable {...{ votes: data, createPercent }} />
    </PageLayout>
  );
};

export default ProposalResultsPage;
