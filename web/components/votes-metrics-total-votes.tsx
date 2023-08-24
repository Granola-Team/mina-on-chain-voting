'use client';

import { Card, CardContent, CardHeader, CardTitle } from 'components/core/card';

interface Props extends React.ComponentProps<typeof Card> {
  total: number;
}

export const VotesMetricsTotalVotes = ({ total, className }: Props) => {
  return (
    <Card className={className}>
      <CardHeader className="pb-0.5">
        <CardTitle className="text-sm font-normal">Total Votes</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="text-xl xl:text-2xl font-bold">{total.toLocaleString('eu-EU')}</div>
        <p className="text-xs text-muted-foreground">Duplicates not included</p>
      </CardContent>
    </Card>
  );
};
