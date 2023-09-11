'use client';

import { Card, CardContent, CardHeader, CardTitle } from 'components/core/card';

interface Props extends React.ComponentProps<typeof Card> {
  total: number;
}

export const VotesMetricsTotalStake = ({ total, className }: Props) => {
  return (
    <Card className={className}>
      <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-0.5">
        <CardTitle className="text-sm font-normal">Total Stake Participated</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="text-xl xl:text-2xl font-bold">
          {Number.parseFloat(total.toFixed(2)).toLocaleString('en-EU')}
        </div>
        <p className="text-xs text-muted-foreground">± 0.005</p>
      </CardContent>
    </Card>
  );
};
