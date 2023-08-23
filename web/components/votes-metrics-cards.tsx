'use client';

import { useTheme } from 'next-themes';

import { useToast } from 'common/hooks/use-toast';
import { GetProposalResult } from 'common/store';
import { ThemePrimaryColor } from 'common/utils';

import { Button } from 'components/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from 'components/card';
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from 'components/dialog';
import { Progress } from 'components/progress';
import { Separator } from 'components/separator';

import { VoteDirection, VoteMetrics } from 'models';
import moment from 'moment';
import { Area, AreaChart, ResponsiveContainer, Tooltip, XAxis } from 'recharts';

import { CopyIcon } from '@radix-ui/react-icons';

export type VotingPeriodProps = {
  startTime: GetProposalResult['start_time'];
  endTime: GetProposalResult['end_time'];
  status: GetProposalResult['status'];
};

export const VotingPeriod = ({ startTime, endTime, status }: VotingPeriodProps) => {
  const now = moment(new Date()).utc();
  const startDate = moment(new Date(startTime)).utc();
  const endDate = moment(new Date(endTime)).utc();
  const duration = moment.duration(endDate.diff(now));
  const nowInMillis = moment().valueOf();
  const percentage = ((nowInMillis - startTime) / (endTime - startTime)) * 100;

  return (
    <Card className="col-span-2 col-start-4 row-start-2">
      <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-0.5">
        <CardTitle>Voting Period</CardTitle>
        <span className="text-xs text-muted-foreground">
          {status === 'Pending' && 'Voting has not started yet'}
          {status === 'In Progress' &&
            `Remaining time: ${duration.days()}D ${duration.hours()}H ${duration.minutes()}m`}
          {status === 'In Review' && 'Results are being verified'}
          {status === 'Completed' && 'Voting has ended'}
        </span>
      </CardHeader>
      <CardContent className="flex flex-col gap-1.5 mt-1.5">
        <Progress className="h-5 rounded-md" value={percentage} />
        <div className="flex justify-between items-center">
          <p className="text-xs text-muted-foreground"> {startDate.format('YYYY-MM-DD | hh:mm A').toString()} UTC</p>
          <p className="text-xs text-muted-foreground"> {endDate.format('YYYY-MM-DD | hh:mm A').toString()} UTC</p>
        </div>
      </CardContent>
    </Card>
  );
};

interface VotingDistributionProps {
  data: Array<VoteMetrics>;
}

export const VotingDistribution = ({ data }: VotingDistributionProps) => {
  const { theme: mode } = useTheme();
  const primaryColor = ThemePrimaryColor[mode === 'dark' ? 'dark' : 'light'];

  return (
    <Card className="col-span-3 row-span-2">
      <CardHeader className="pb-0">
        <CardTitle>Voting Distribution</CardTitle>
        <CardDescription>
          Track the distribution of <span className="inline font-semibold">FOR / AGAINST</span> votes.
        </CardDescription>
      </CardHeader>
      <CardContent>
        <div className="h-40">
          <ResponsiveContainer width="100%" height="100%">
            <AreaChart data={data}>
              <XAxis dataKey="DATE" height={16} tick={{ fontSize: 12 }} />
              <Area
                type="linear"
                dataKey="FOR"
                strokeOpacity={0.4}
                stroke="grey"
                fillOpacity={1}
                fill={`hsl(${primaryColor})`}
              />
              <Area
                type="linear"
                dataKey="AGAINST"
                strokeOpacity={0.25}
                stroke="grey"
                fillOpacity={0.75}
                fill={`hsl(${primaryColor}) / 0.2`}
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

interface VotingInstructionsProps {
  memo: GetProposalResult['key'];
}

export const VotingInstructions = ({ memo }: VotingInstructionsProps) => {
  return (
    <Card className="col-start-5">
      <CardHeader className="space-y-0 pb-1">
        <CardTitle className="text-sm font-normal">How do I cast my vote?</CardTitle>
        <p className="text-xs text-muted-foreground">I want to vote...</p>
      </CardHeader>
      <CardContent className="grid grid-cols-2 gap-4 mt-1.5">
        <VotingInstructionsDialog {...{ memo, variant: 'FOR' }} />
        <VotingInstructionsDialog {...{ memo, variant: 'AGAINST' }} />
      </CardContent>
    </Card>
  );
};

const VotingInstructionsDialog = ({ memo, variant }: VotingInstructionsProps & { variant: VoteDirection }) => {
  const { toast } = useToast();

  const isFor = variant === 'FOR';
  const displayMemo = isFor ? memo : `no ${memo}`;

  return (
    <Dialog>
      <DialogTrigger asChild>
        <Button size="sm">{isFor ? 'For' : 'Against'}</Button>
      </DialogTrigger>
      <DialogContent>
        <DialogHeader className="space-y-3.5">
          <DialogTitle className="text-center">How do I cast my vote?</DialogTitle>
          <Separator />
          <DialogDescription className="flex flex-col gap-3">
            <span className="text-center">
              To cast your vote you need to send yourself a transaction with the following keyword(s) in the memo field:
            </span>

            <Button
              variant="outline"
              size="lg"
              className="gap-2"
              onClick={() => {
                navigator.clipboard.writeText(displayMemo);
                toast({
                  title: 'Copied to Clipboard! 🎉',
                  description: `Send a transaction to cast your vote.`,
                });
              }}
            >
              <span className="text-xl text-center font-semibold tracking-tight text-foreground">{displayMemo}</span>
              <CopyIcon />
            </Button>
          </DialogDescription>
        </DialogHeader>
      </DialogContent>
    </Dialog>
  );
};

interface VotingTotalProps {
  total: number;
}

export const VotingTotal = ({ total }: VotingTotalProps) => {
  return (
    <Card className="col-start-4">
      <CardHeader className="pb-0.5">
        <CardTitle className="text-sm font-normal">Total Votes</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="text-2xl font-bold">{total.toLocaleString('eu-EU')}</div>
        <p className="text-xs text-muted-foreground">Duplicates not included</p>
      </CardContent>
    </Card>
  );
};

interface VotingTotalStakeProps {
  total: number;
}

export const VotingTotalStake = ({ total }: VotingTotalStakeProps) => {
  return (
    <Card className="col-start-5">
      <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-0.5">
        <CardTitle className="text-sm font-normal">Total Stake Participated</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="text-2xl font-bold">{Number.parseFloat(total.toFixed(2)).toLocaleString('en-EU')}</div>
        <p className="text-xs text-muted-foreground">± 0.005</p>
      </CardContent>
    </Card>
  );
};
