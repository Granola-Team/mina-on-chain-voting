import { Stack } from '@mui/material';

import { Instructions, PageLayout, TotalVotes, Vote, VotesTable, VotingPeriod } from 'components/v1';

//! Dummy data
const data = [
  {
    id: 1,
    account: 'B62qj4sYygXUKhKKitLtbmrxN1PWx7Xd7C5R2tyqWtSGA5bctZAuVDw',
    hash: 'CkpZxqP7pM5nRTy9dcJaCsLGxg1iFYLstr1zGaScwjfV1iBbgW8FY',
    memo: 'no cftest-2',
    height: 212888,
    status: 'Pending',
    timestamp: 1675989172,
  },
  {
    id: 2,
    account: 'B62qj4sYygXUKhKKitLtbmrxN1PWx7Xd7C5R2tyqWtSGA5bctZAuVDw',
    hash: 'CkpZxqP7pM5nRTy9dcJaCsLGxg1iFYLstr1zGaScwjfV1iBbgW8FY',
    memo: 'no cftest-2',
    height: 212888,
    status: 'Canonical',
    timestamp: 1675989172,
  },
  {
    id: 3,
    account: 'B62qj4sYygXUKhKKitLtbmrxN1PWx7Xd7C5R2tyqWtSGA5bctZAuVDw',
    hash: 'CkpZxqP7pM5nRTy9dcJaCsLGxg1iFYLstr1zGaScwjfV1iBbgW8FY',
    memo: 'no cftest-2',
    height: 212888,
    status: 'Canonical',
    timestamp: 1675989172,
  },
] satisfies Vote[];

const HomePage = () => {
  return (
    <PageLayout>
      <Stack direction="row" spacing={1}>
        <Instructions keyword="cftest-2" />
        <TotalVotes totalVotes={data.length} />
      </Stack>
      <VotingPeriod
        startDate={new Date(2022, 0, 15, 8, 30, 0)}
        endDate={new Date(2023, 0, 15, 8, 30, 0)}
        queryDate={new Date(2023, 0, 15, 8, 30, 0)}
      />
      <VotesTable votes={data} />
    </PageLayout>
  );
};

export default HomePage;
