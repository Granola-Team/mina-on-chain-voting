'use client';

import { useToast } from 'common/hooks/use-toast';
import { GetProposalResult } from 'common/store';

import { Button } from 'components/core/button';
import { Card, CardContent, CardHeader, CardTitle } from 'components/core/card';
import {
  Dialog as DialogBase,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from 'components/core/dialog';
import { Separator } from 'components/core/separator';

import { VoteDirection } from 'models';

import { CopyIcon } from '@radix-ui/react-icons';

interface Props extends React.ComponentProps<typeof Card> {
  memo: GetProposalResult['key'];
}

export const VotesMetricsInstructions = ({ memo, className }: Props) => {
  return (
    <Card className={className}>
      <CardHeader className="space-y-0 pb-1">
        <CardTitle className="text-sm font-normal">How do I cast my vote?</CardTitle>
        <p className="text-xs text-muted-foreground">I want to vote...</p>
      </CardHeader>
      <CardContent className="grid grid-cols-2 gap-4 mt-1">
        <Dialog {...{ memo, variant: 'FOR' }} />
        <Dialog {...{ memo, variant: 'AGAINST' }} />
      </CardContent>
    </Card>
  );
};

const Dialog = ({ memo, variant }: Props & { variant: VoteDirection }) => {
  const { toast } = useToast();

  const isFor = variant === 'FOR';
  const displayMemo = isFor ? memo : `no ${memo}`;

  return (
    <DialogBase>
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
              test-id="dialog-copy-button"
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
    </DialogBase>
  );
};
