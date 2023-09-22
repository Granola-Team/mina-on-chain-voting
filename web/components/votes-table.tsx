'use client';

import type { ForwardRefExoticComponent, RefAttributes } from 'react';

import { GetProposalResult, GetProposalResultsResult } from 'common/store';

import { Badge } from 'components/core/badge';
import { DataTable } from 'components/core/data-table';
import { DataTableColumnHeader } from 'components/core/data-table-column-header';
import { HoverCard, HoverCardContent, HoverCardTrigger } from 'components/core/hover-card';
import { VotesTableToolbar } from 'components/votes-table-toolbar';

import { VoteStatus } from 'models';
import moment from 'moment';

import { CheckCircledIcon, CheckIcon, CountdownTimerIcon, Cross2Icon, CrossCircledIcon } from '@radix-ui/react-icons';
import type { IconProps } from '@radix-ui/react-icons/dist/types';
import { ColumnDef } from '@tanstack/react-table';

interface Props {
  votes: GetProposalResult['votes'] | GetProposalResultsResult['votes'];
}

interface VotesTableStatus {
  value: VoteStatus;
  icon: ForwardRefExoticComponent<IconProps & RefAttributes<SVGSVGElement>>;
}

interface VotesTableDirection {
  value: 'FOR' | 'AGAINST';
  icon: ForwardRefExoticComponent<IconProps & RefAttributes<SVGSVGElement>>;
}

export const votesTableStatuses = [
  {
    value: 'Pending',
    icon: CountdownTimerIcon,
  },
  {
    value: 'Orphaned',
    icon: CrossCircledIcon,
  },
  {
    value: 'Canonical',
    icon: CheckCircledIcon,
  },
] satisfies VotesTableStatus[];

export const votesTableDirections = [
  {
    value: 'FOR',
    icon: CheckIcon,
  },
  {
    value: 'AGAINST',
    icon: Cross2Icon,
  },
] satisfies VotesTableDirection[];

const columns: ColumnDef<Props['votes'][number]>[] = [
  {
    accessorKey: 'height',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Height" className="pl-2.5" />,
    cell: ({ row }) => {
      return (
        <div className="flex pl-2.5">
          <span className="whitespace-nowrap text-xs font-semibold">{row.getValue('height')}</span>
        </div>
      );
    },
    enableHiding: false,
    enableSorting: false,
  },
  {
    accessorKey: 'timestamp',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Timestamp" />,
    cell: ({ row }) => {
      const timestamp = moment(new Date(row.getValue('timestamp'))).utc();
      return (
        <span className="whitespace-nowrap text-xs font-semibold">
          {timestamp.format('YYYY-MM-DD - hh:mm').toString()}
        </span>
      );
    },
    enableHiding: false,
    enableSorting: false,
  },
  {
    accessorKey: 'account',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Account" className="pl-[1px]" />,
    cell: ({ row }) => {
      const account = row.getValue('account') as string;
      const prefix = account.slice(0, 4);
      const suffix = account.slice(-4);

      return (
        <HoverCard openDelay={200} closeDelay={100}>
          <HoverCardTrigger>
            <Badge variant="outline" className="bg-gray-200/10 dark:bg-gray-500/10">
              <span className="select-none">{`${prefix}...${suffix}`}</span>
            </Badge>
          </HoverCardTrigger>
          <HoverCardContent className="w-fit">
            <span className="text-xs font-medium">{account}</span>
          </HoverCardContent>
        </HoverCard>
      );
    },
    enableHiding: false,
    enableSorting: false,
  },
  {
    accessorKey: 'hash',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Hash" />,
    cell: ({ row }) => {
      return (
        <div className="w-[500px]">
          <span className="text-xs font-semibold select-none">{row.getValue('hash')}</span>
        </div>
      );
    },
    enableHiding: false,
    enableSorting: false,
  },
  {
    accessorKey: 'direction',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Vote" />,
    cell: ({ row }) => {
      const vote = votesTableDirections.find((direction) => direction.value === row.getValue('direction'));

      if (!vote) {
        return null;
      }

      return (
        <div className="w-20 flex items-center gap-1">
          <vote.icon />
          <span className="text-xs font-medium">{vote.value}</span>
        </div>
      );
    },
    enableHiding: false,
    enableSorting: false,
    filterFn: (row, id, value) => {
      return value.includes(row.getValue(id));
    },
  },
  {
    accessorKey: 'status',
    header: ({ column }) => <DataTableColumnHeader column={column} title="Status" />,
    cell: ({ row }) => {
      const status = row.getValue('status');

      switch (status) {
        case 'Canonical':
          return (
            <Badge className="bg-green-600/70 dark:bg-green-500/20 dark:border-green-200/20 border rounded-lg select-none">
              {status}
            </Badge>
          );
        case 'Pending':
          return (
            <Badge className="bg-yellow-600/70 dark:bg-yellow-400/30 dark:border-yellow-300/40 border rounded-lg select-none">
              {status}
            </Badge>
          );
        case 'Orphaned':
          return (
            <Badge className="bg-red-600/80 dark:bg-red-500/30 border-red-300/10 border rounded-lg select-none">
              {status}
            </Badge>
          );
        default:
          return null;
      }
    },
    enableHiding: false,
    enableSorting: false,
    filterFn: (row, id, value) => {
      return value.includes(row.getValue(id));
    },
  },
];

export const VotesTable = ({ votes }: Props) => {
  return <DataTable data={votes} columns={columns} Toolbar={VotesTableToolbar} variant="vote" />;
};
