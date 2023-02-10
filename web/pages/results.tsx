import { PageLayout, ResultsOverview, ResultsTable, VoteResult, VotingPeriod, VotingResults } from 'components/v1';

//! Dummy Data
const startDate = new Date(2023, 0, 15, 8, 30, 0);
const endDate = new Date(2023, 4, 15, 8, 30, 0);
const queryDate = new Date(2023, 1, 10, 8, 30, 0);

const data = [
  {
    id: 1,
    account: 'B62qj4sYygXUKhKKitLtbmrxN1PWx7Xd7C5R2tyqWtSGA5bctZAuVDw',
    hash: 'CkpZxqP7pM5nRTy9dcJaCsLGxg1iFYLstr1zGaScwjfV1iBbgW8FY',
    memo: 'cftest-2',
    height: 212888,
    status: 'Canonical',
    timestamp: 1675989172,
    stake_weight: 1356.237,
  },
  {
    id: 2,
    account: 'B62qj4sYygXUKhKKitLtbmrxN1PWx7Xd7C5R2tyqWtSGA5bctZAuVDw',
    hash: 'CkpZxqP7pM5nRTy9dcJaCsLGxg1iFYLstr1zGaScwjfV1iBbgW8FY',
    memo: 'no cftest-2',
    height: 212888,
    status: 'Pending',
    timestamp: 1675989172,
    stake_weight: 1256,
  },
  {
    id: 3,
    account: 'B62qj4sYygXUKhKKitLtbmrxN1PWx7Xd7C5R2tyqWtSGA5bctZAuVDw',
    hash: 'CkpZxqP7pM5nRTy9dcJaCsLGxg1iFYLstr1zGaScwjfV1iBbgW8FY',
    memo: 'no cftest-2',
    height: 212888,
    status: 'Pending',
    timestamp: 1675989172,
    stake_weight: 1222,
  },
] satisfies VoteResult[];

const useStakeStatistics = (votes: VoteResult[]) => {
  const stakeWeight = () => {
    let positiveStakeWeight = 0;
    let negativeStakeWeight = 0;

    votes.forEach((vote) => {
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

const ResultsPage = () => {
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

export default ResultsPage;
