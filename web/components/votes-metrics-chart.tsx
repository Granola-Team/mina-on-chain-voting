'use client';

import { useTheme } from 'next-themes';

import { Card, CardContent, CardDescription, CardHeader, CardTitle } from 'components/core/card';

import { VoteMetrics } from 'models';
import { Area, AreaChart, ResponsiveContainer, Tooltip, XAxis } from 'recharts';

interface Props extends React.ComponentProps<typeof Card> {
  data: Array<VoteMetrics>;
}

export const VotesMetricsChart = ({ data, className }: Props) => {
  useTheme();

  return (
    <Card className={className}>
      <CardHeader className="pb-0">
        <CardTitle>Voting Distribution</CardTitle>
        <CardDescription>
          Track the distribution of <span className="inline font-semibold">FOR / AGAINST</span> votes.
        </CardDescription>
      </CardHeader>
      <CardContent className="px-3 md:px-6">
        <div className="h-40">
          <ResponsiveContainer width="100%" height="100%">
            <AreaChart data={data}>
              <XAxis dataKey="DATE" height={16} className="text-xs text-muted-foreground" />
              <Area type="linear" dataKey="FOR" strokeOpacity={0.4} stroke="grey" fillOpacity={1} fill="#EDA13A" />
              <Area
                type="linear"
                dataKey="AGAINST"
                strokeOpacity={0.25}
                stroke="grey"
                fillOpacity={0.75}
                fill="#EDA13A bg-opacity-75"
              />
              <Tooltip
                content={({ active, payload }) => {
                  if (active && payload && payload.length) {
                    return (
                      <div className="rounded-lg border bg-background p-2 shadow-sm">
                        <div className="grid grid-cols-2 gap-2">
                          <div className="flex flex-col">
                            <span className="text-[0.70rem] uppercase text-muted-foreground">For</span>
                            <span className="font-bold">{payload[0].value}</span>
                          </div>
                          <div className="flex flex-col">
                            <span className="text-[0.70rem] uppercase text-muted-foreground">Against</span>
                            <span className="font-bold text-muted-foreground">{payload[1].value}</span>
                          </div>
                        </div>
                      </div>
                    );
                  }

                  return null;
                }}
              />
            </AreaChart>
          </ResponsiveContainer>
        </div>
      </CardContent>
    </Card>
  );
};
