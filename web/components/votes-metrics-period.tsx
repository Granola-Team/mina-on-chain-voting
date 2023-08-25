'use client';

import { GetProposalResult } from 'common/store';

import { Card, CardContent, CardHeader, CardTitle } from 'components/core/card';
import { Progress } from 'components/core/progress';

import moment from 'moment';

interface Props extends React.ComponentProps<typeof Card> {
  startTime: GetProposalResult['start_time'];
  endTime: GetProposalResult['end_time'];
  status: GetProposalResult['status'];
}

export const VotesMetricsPeriod = ({ startTime, endTime, status, className }: Props) => {
  const now = moment(new Date()).utc();
  const startDate = moment(new Date(startTime)).utc();
  const endDate = moment(new Date(endTime)).utc();
  const duration = moment.duration(endDate.diff(now));
  const nowInMillis = moment().valueOf();
  const percentage = ((nowInMillis - startTime) / (endTime - startTime)) * 100;

  return (
    <Card className={className}>
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
        <Progress
          className="h-5 rounded-md"
          value={percentage === -Infinity ? 0 : percentage > 100 ? 100 : percentage}
        />
        <div className="flex justify-between items-center">
          <p className="text-xs text-muted-foreground"> {startDate.format('YYYY-MM-DD | hh:mm A').toString()} UTC</p>
          <p className="text-xs text-muted-foreground"> {endDate.format('YYYY-MM-DD | hh:mm A').toString()} UTC</p>
        </div>
      </CardContent>
    </Card>
  );
};
