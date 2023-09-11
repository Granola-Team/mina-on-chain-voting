'use client';

import { GetProposalResultsResult } from 'common/store';

import { Card, CardContent, CardHeader, CardTitle } from 'components/core/card';
import { Progress } from 'components/core/progress';

interface Props extends React.ComponentProps<typeof Card> {
  total: GetProposalResultsResult['total_stake_weight'];
  positive: GetProposalResultsResult['positive_stake_weight'];
  negative: GetProposalResultsResult['negative_stake_weight'];
}

export const VotesMetricsResults = ({ total, positive, negative, className }: Props) => {
  const positivePercentage = total > 0 ? ((positive / total) * 100).toFixed(4).replace(/\.0+$/, '') : '0';
  const negativePercentage = total > 0 ? ((negative / total) * 100).toFixed(4).replace(/\.0+$/, '') : '0';

  return (
    <Card className={className}>
      <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-0.5">
        <CardTitle>Voting Results</CardTitle>
        <span className="text-xs text-muted-foreground">Voting has ended</span>
      </CardHeader>
      <CardContent className="flex flex-col gap-1.5 mt-1.5">
        <Progress className="h-5 rounded-md" value={Number.parseFloat(positivePercentage)} />
        <div className="flex justify-between items-center">
          <p className="text-xs text-muted-foreground font-medium">FOR - {positivePercentage}%</p>
          <p className="text-xs text-muted-foreground font-medium">AGAINST - {negativePercentage}%</p>
        </div>
      </CardContent>
    </Card>
  );
};
